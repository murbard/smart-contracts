import math
from collections import defaultdict

class Tick(object):
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
        self.ΔL = 0
        self.feeGrowthOutside = feeGrowthOutside
        self.n_positions = 0

class Position(object):
    def __init__(self, L = 0):
        self.L = L
        self.feeGrowthInsideLast = XY()

class XY(object):
    def __init__(self, x = 0, y = 0):
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


class Contract(object):
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
        return math.floor(math.log(srp) / math.log(math.sqrt(1.0001)))

    @staticmethod
    def srp(tick):
        """
        Computes the square root of the price corresponding to a given tick
        :param tick: the index of a tick
        :return: the corresponding square root price
        """
        return math.pow(math.sqrt(1.0001), tick)

    def __init__(self, X=0, Y=0, fee=0.3 / 100):
        self.balance = XY(X, Y)
        self.srP = math.sqrt(Y / X)
        self.i_a = self.tick(self.srP)
        self.L = math.floor(math.sqrt(X * Y))
        self.fee = fee
        infinity = 10 ** 100
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
            self.ticks[i] = Tick(i_l, i_next, self.feeGrowth if self.i_a >= i else XY(0, 0))
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

    def set_position(self, user, i_l, i_l_l, i_u, i_u_l, ΔL):

        assert (i_l_l < i_l)
        if i_l not in self.ticks:
            self.initialize_tick(i_l, i_l_l)
        assert (i_u_l < i_u)
        if i_u not in self.ticks:
            self.initialize_tick(i_u, i_u_l)

        position_key = (user, i_l, i_u)
        self.positions[position_key].L += ΔL
        assert(self.positions[position_key].L >= 0)
        #todo, garbage collect if we are unwinding the position completely

        fees = self.collect_fees(user, i_l, i_u)

        Δ = XY()
        # Add or remove liquidity above the current tick
        if self.i_a < i_l:
            Δ.x = ΔL * (1 / self.srp(i_l) - 1 / self.srp(i_u))
            Δ.y = 0
        # Add or remove liquidity around the current tick
        elif i_l <= self.i_a < i_u:
            Δ.x = ΔL * (1 / self.srP - 1 / self.srp(i_u))
            Δ.y = ΔL * (self.srP - self.srp(i_l))
            self.L += ΔL
        else:  # i_a >= i_u
            Δ.x = 0
            Δ.y = ΔL * (self.srp(i_u) - self.srp(i_l))

        Δ -= fees

        # make a note of how much liquidity is gained or lost when
        # entering this interval
        self.ticks[i_l].ΔL += ΔL
        self.ticks[i_u].ΔL -= ΔL

        self.balance += Δ
        return -Δ

    def X_to_Y(self, ΔX):
        assert(ΔX >= 0)
        # collect fee
        feeX = ΔX * self.fee
        self.balance.X += feeX
        self.feeGrowth.X += feeX / self.L
        return self.X_to_Y_no_fees(ΔX - feeX)

    def X_to_Y_no_fees(self, ΔX):
        srp_new = 1 / (1 / self.srp - ΔX / self.L)
        greater_ticks = [t for t in self.ticks.keys() if t > self.i_a]
        if len(greater_ticks) == 0:  # not more liquidity left
            raise Exception("Not enough liquidity")
        i_u = min(greater_ticks)

        if self.tick(srp_new) < i_u:  # we didn't pushed past the interval
            δY = self.L * (self.srP - self.srp_new)
            self.srP = srp_new
            self.i_a = self.tick(self.srP)
            self.X += ΔX
            self.Y -= δY
            return XY(-ΔX, δY)
        else:
            # compute what we got up til i_u and how much it cost
            # well, what δX would have taken me there?
            srP_u = self.srp(i_u)
            δX = self.L * (1 / srP_u - 1 / self.srP)
            δY = self.L * (self.srP_u - self.srP)

            # remove the liquidity we used to have
            self.L += self.ticks[i_u].ΔL
            # flip feeGrowth
            self.ticks[i_u].feeGrowthOutside = self.feeGrowth - self.ticks[i_u].feeGrowthOutside

            self.i_a = i_u
            self.srP = self.srp(self.i_a)
            self.X += δX
            self.Y -= δY
            return self.X_to_Y_no_fees(ΔX - δX)

    def Y_to_X(self, ΔY):
        assert (ΔY >= 0)
        # collect fee
        feeY = ΔY * self.fee
        self.balance.Y += feeY
        self.feeGrowth.Y += feeY / self.L
        return self.X_to_Y_no_fees(ΔY - feeY)

    def Y_to_X_no_fees(self, y_in):
        assert(y_in >= 0)
        feeY = y_in * self.fee
        ΔY = y_in - feeY
        srp_new = self.srP + y_in / self.L
        smaller_ticks = [t for t in self.ticks.keys() if t <= self.i_a]
        if len(smaller_ticks) == 0:  # no more liquidity left
            raise Exception("Not enough liquidity")
        i_l = max(smaller_ticks)

        if self.tick(srp_new) >= i_l:  # we did not push past the interval
            δX = self.L * (1 / self.srP - self.srp_new)
            self.srP = srp_new
            self.i_a = self.tick(self.srP)
            self.X -= δX
            self.Y += ΔY
            return XY(δX,-ΔY)
        else:
            srP_l = self.srp(i_l)
            δX = self.L * (1 / srP_l - 1 / self.srP)
            δY = self.L * (self.srP - self.srP_l)
            self.L -= self.ticks[i_l].ΔL
            self.i_a = i_l - 1
            self.srP = self.srp(self.i_l) - 1e-16 # todo can we do better than this crutch?
            self.X -= δX
            self.Y += δY
            return self.Y_to_X_no_fees(ΔY - δY)