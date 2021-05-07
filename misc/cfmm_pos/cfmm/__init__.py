import math
from collections import defaultdict
from cfmm.data import AutoRepr

infinity = 10 ** 100

class Tick(AutoRepr):
    """
    An initialized tick, marking the beginning or end of a position
    """

    def __init__(self, i_prev, i_next, feeGrowthOutside):
        """
        :type i_prev: int
        :type i_next: int
        """
        self.i_prev = i_prev
        self.i_next = i_next
        self.Delta_L = 0
        self.feeGrowthOutside = feeGrowthOutside
        self.n_positions = 0


class Position(AutoRepr):
    """
    A LP's position
    """
    def __init__(self, L=0):
        self.L = L
        self.feeGrowthInsideLast = XY()

class XY(AutoRepr):
    """
    A pair of balances in asset X and Y
    """
    def __init__(self, x=0, y=0):
        self.x, self.y = x, y

    def __add__(self, other):
        x = self.x + other.x
        y = self.y + other.y
        return XY(x, y)

    def __sub__(self, other):
        x = self.x - other.x
        y = self.y - other.y
        return XY(x, y)

    def __neg__(self):
        return XY(-self.x, -self.y)

    def __mul__(self, other):
        return XY(other * self.x, other * self.y)

    def __eq__(self, other):
        return isinstance(other, XY) and self.x == other.x and self.y == other.y


class Contract(AutoRepr):
    """
    A contract in the fashion of Uniswap v3
    """

    @staticmethod
    def tick(srp):
        """
        Computes the closest tick index below a certain price, given its square root
        :param srp: square root of a price
        :return: the closest tick below a certain price
        """
        if srp == infinity:
            return infinity
        else:
            return math.floor(math.log(srp) / math.log(math.sqrt(1.0001)))

    @staticmethod
    def srp(tick):
        """
        Computes the square root of the price corresponding to a given tick
        :param tick: the index of a tick
        :return: the corresponding square root price
        """
        if tick == infinity:
            return infinity
        return math.pow(math.sqrt(1.0001), tick)

    def __init__(self, X, Y, fee=0.3 / 100):
        self.balance = XY(X, Y)
        self.srP = math.sqrt(Y / X)
        self.i_a = self.tick(self.srP)
        self.L = math.floor(math.sqrt(X * Y))
        self.fee = fee
        self.i_l = -infinity
        self.ticks = {-infinity: Tick(-infinity, infinity, XY()), infinity: Tick(-infinity, infinity, XY())}
        self.positions = defaultdict(Position)
        self.feeGrowth = XY()

    def initialize_tick(self, i, i_l):
        """
        Initialize a new tick at index i, provide the index of an initialized tick lower
        than i to find it easily in the linked list. Assumes that i is *not* already initialized.
        :param i:
        :param i_l:
        """
        assert (i not in self.ticks)
        assert (i_l < i)
        i_next = self.ticks[i_l].i_next

        if i_next > i:
            self.ticks[i_l].i_next = i
            # find an instance where i_a = i and we set XY(0, 0) and that's wrong
            self.ticks[i] = Tick(i_l, i_next, self.feeGrowth if self.i_a >= i else XY())
            self.ticks[i_next].i_prev = i
        else:
            self.initialize_tick(i, i_next)

    def collect_fees(self, user, i_l, i_u):
        key = (user, i_l, i_u)
        position = self.positions[key]
        f_a = self.feeGrowth - self.ticks[i_u].feeGrowthOutside if self.i_a >= i_u else self.ticks[i_u].feeGrowthOutside
        f_b = self.ticks[i_l].feeGrowthOutside if self.i_a >= i_l else self.feeGrowth - self.ticks[i_l].feeGrowthOutside
        feeGrowthInside = self.feeGrowth - f_a - f_b

        fees = (feeGrowthInside - position.feeGrowthInsideLast) * position.L
        position.feeGrowthInsideLast = feeGrowthInside
        return fees

    def set_position(self, user, i_l, i_l_l, i_u, i_u_l, Delta_L):

        assert (i_l_l <= i_l)
        if i_l not in self.ticks:
            self.initialize_tick(i_l, i_l_l)
        assert (i_u_l <= i_u)
        if i_u not in self.ticks:
            self.initialize_tick(i_u, i_u_l)

        position_key = (user, i_l, i_u)
        fees = self.collect_fees(user, i_l, i_u)

        self.positions[position_key].L += Delta_L
        assert (self.positions[position_key].L >= 0)
        # todo, garbage collect if we are unwinding the position completely?

        Delta = XY()
        # Add or remove liquidity above the current tick
        if self.i_a < i_l:
            Delta.x = Delta_L * (1 / self.srp(i_l) - 1 / self.srp(i_u))
            Delta.y = 0
        # Add or remove liquidity around the current tick
        elif i_l <= self.i_a < i_u:
            # update interval we are in if need be
            if i_l > self.i_l:
                self.i_l = i_l
            Delta.x = Delta_L * (1 / self.srP - 1 / self.srp(i_u))
            Delta.y = Delta_L * (self.srP - self.srp(i_l))
            self.L += Delta_L
        else:  # i_a >= i_u
            Delta.x = 0
            Delta.y = Delta_L * (self.srp(i_u) - self.srp(i_l))

        Delta -= fees

        # make a note of how much liquidity is gained or lost when
        # entering this interval
        self.ticks[i_l].Delta_L += Delta_L
        self.ticks[i_u].Delta_L -= Delta_L

        self.balance += Delta
        return -Delta


    def X_to_Y(self, dX, fee = None):
        # dX must be positive
        assert(dX >= 0)
        if fee is None:
            fee = self.fee
        # If there is no liquidity, stop the trade at this point
        if self.L == 0:
            self.i_a = self.tick(self.srP) # we may need to update i_a if we went through several ticks to reach this point
            return XY()
        # Assume the trade will fit in a tick, what would the fees be like?
        fees = XY(dX * fee, 0)
        srp_new = 1.0 / (1.0 / self.srP + (dX - fees.x) / self.L)
        i_l = self.i_l
        tick_new = self.tick(srp_new)
        if tick_new >= i_l:  # we didn't pushed past the interval
            dY = - (dX - fees.x) * self.srP * srp_new
            self.srP = srp_new
            self.i_a = tick_new
            user = XY(-dX, -dY)
            self.balance -= user
            # Update fee growth with the fees we just collected
            self.feeGrowth += fees * (1.0 / self.L)
            return user
        else:
            # compute what we got up til i_u and how much it cost
            # well, what delta_X would have taken me there?
            self.i_l = self.ticks[self.i_l].i_prev
            srP_l = self.srp(i_l)
            dY = self.L * (srP_l - self.srP)
            dX_ = - dY / (self.srP * srP_l)
            tmp = dX_ / (1.0 - fee)
            dX_, fees = tmp, XY(tmp - dX_, 0)
            # update fee growth
            self.feeGrowth += fees * (1.0 / self.L)

            # remove the liquidity we used to have
            self.L -= self.ticks[i_l].Delta_L
            # flip feeGrowth
            self.ticks[i_l].feeGrowthOutside = self.feeGrowth - self.ticks[i_l].feeGrowthOutside
            self.srP = self.srp(i_l) - 1e-16  # todo can we do better than this crutch?
            user = XY(-dX_, -dY)
            self.balance -= user
            return user + self.X_to_Y(dX - dX_, fee)

    def Y_to_X(self, dY, fee = None):
        # dY must be positive
        assert (dY >= 0)
        if fee is None:
            fee = self.fee
        # If there is no liquidity, stop the trade at this point
        if self.L == 0:
            self.i_a = self.tick(self.srP)  # we may need to update i_a if we went through several ticks to reach this point
            return XY()
        # Assume the trade will fit in a tick, what would the fees be like?
        fees = XY(0, dY * fee)
        srp_new = self.srP + (dY - fees.y) / self.L
        i_u = self.ticks[self.i_l].i_next
        tick_new = self.tick(srp_new)

        if tick_new < i_u:  # we did not push past the interval
            dX = - (dY - fees.y) / (self.srP * srp_new)
            self.srP = srp_new
            self.i_a = tick_new
            user = XY(-dX, -dY)
            self.balance -= user
            # Update fee growth with the fees we just collected
            self.feeGrowth += fees * (1.0 / self.L)
            return user
        else:
            self.i_l = i_u
            srP_u = self.srp(i_u)
            dY_ = self.L * (srP_u - self.srP)
            dX = - dY_ / (self.srP * srP_u)
            tmp = dY_ / (1.0 - fee)
            dY_, fees = tmp, XY(0, tmp - dY_)
            # update fee growth
            self.feeGrowth += fees * (1.0 / self.L)
            self.L += self.ticks[i_u].Delta_L
            self.ticks[i_u].feeGrowthOutside = self.feeGrowth - self.ticks[i_u].feeGrowthOutside

            self.srP = srP_u
            user = XY(-dX, -dY_)
            self.balance -= user
            return user + self.Y_to_X(dY - dY_, fee)
