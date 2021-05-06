import unittest
from cfmm import *

class TestTick(unittest.TestCase):

    def test_init(self):
        tick = Tick(7, 13, 0)
        self.assertEqual(tick.i_prev, 7)
        self.assertEqual(tick.i_next, 13)
        self.assertEqual(tick.feeGrowthOutside, 0)
        self.assertEqual(tick.n_positions, 0)


class TestXY(unittest.TestCase):

    def test_arithmetic(self):
        a = XY(3, 6)
        b = XY(4, -2)
        self.assertNotEqual(a, b)
        self.assertNotEqual(XY(1,2), XY(1,5))
        self.assertNotEqual(XY(1, 2), XY(3, 2))
        self.assertEqual(a, XY(a.x, a.y))
        c = XY(a.x + b.x, a.y + b.y)
        self.assertEqual(a + b, c)
        c = XY(a.x - b.x, a.y - b.y)
        self.assertEqual(a - b, c)
        c = XY(- b.x, - b.y)
        self.assertEqual(-b, c)
        c = XY(7 * a.x, 7 * a.y)
        self.assertEqual(a * 7, c)


class TestPosition(unittest.TestCase):
    def test_init(self):
        p = Position()
        self.assertEqual(p.L, 0)

class TestContract(unittest.TestCase):

    def setUp(self):
        self.contract = Contract(X=0.0001, Y=0.0001)
        self.infinity = 10 ** 100

    def test_init(self):
        X, Y, fee = 10, 50, 0.25 / 100
        contract = Contract(X = X, Y = Y, fee = fee)
        self.assertEqual(contract.balance, XY(X, Y))
        self.assertEqual(contract.balance.x, X)
        self.assertEqual(contract.balance.y, Y)
        self.assertAlmostEqual(contract.srP ** 2, Y / X)
        self.assertEqual(contract.fee, fee)
        self.assertEqual(len(contract.ticks), 2)
        self.assertEqual(len(contract.positions), 0)
        self.assertEqual(contract.feeGrowth, XY(0, 0))

    def test_tick(self):
        #self.subTest(i=0)
        self.assertEqual(Contract.tick(2.71814592682),20000)
        #self.subTest(i=1)
        self.assertEqual(Contract.tick(2.71814592681), 19999)
        #self.subTest(i=2)
        self.assertEqual(Contract.tick(0.99), -202)
        self.assertEqual(Contract.tick(1), 0)

    def test_srp(self):
        self.assertAlmostEqual(Contract.srp(10), 1.00050010001000050001)
        self.assertAlmostEqual(Contract.srp(-10), 0.9995001499650069987)
        self.assertEqual(Contract.srp(0), 1)

    def ticks_is_linked_list(self, contract):
        infinity = 10 ** 100
        i = -infinity
        i_prev = -infinity
        while i != infinity:
            self.assertEqual(contract.ticks[i].i_prev, i_prev)
            i_prev = i
            i = contract.ticks[i].i_next
        self.assertEqual(contract.ticks[i].i_prev, i_prev)
        self.assertEqual(contract.ticks[i].i_next, infinity)

    def feegrowth_coherent(self):
        fees = XY()
        for i, tick in self.contract.ticks.items():
            next_tick = self.contract.ticks[tick.i_next]
            fa = self.contract.feeGrowth - next_tick.feeGrowthOutside if self.contract.i_a >= tick.i_next else next_tick.feeGrowthOutside
            fb = tick.feeGrowthOutside if self.contract.i_a >= i else self.contract.feeGrowth - tick.feeGrowthOutside
            fee = self.contract.feeGrowth - fa - fb
            fees += fee

        expected_fees = self.contract.feeGrowth
        self.assertAlmostEqual(fees.x, expected_fees.x)
        self.assertAlmostEqual(fees.y, expected_fees.y)

    def test_linked_list(self):
        return self.ticks_is_linked_list(self.contract)

    def test_initialize_tick(self):
        i = 0
        i_l = -self.infinity
        self.contract.initialize_tick(i, i_l)
        self.assertEqual(len(self.contract.ticks), 3)
        self.assertEqual(self.contract.ticks[0].i_prev, -self.infinity)
        self.assertEqual(self.contract.ticks[0].i_next, self.infinity)
        self.ticks_is_linked_list(self.contract)
        self.assertRaises(AssertionError, self.contract.initialize_tick, i = -17, i_l = -17)
        # todo store corresponding square root price

    def test_set_position(self):
        i_l, i_u = 10, 50
        self.contract.set_position("alice", i_l, -self.infinity, i_u, -self.infinity, 42)
        # price should be 1, so i_a = 0 so we should be out of the liquidity region which
        # should be between -infinity and 0
        self.assertEqual(self.contract.L, 0)
        self.assertEqual(self.contract.ticks[i_l].Delta_L, 42)
        self.assertEqual(self.contract.ticks[i_u].Delta_L, -42)
        i_l, i_u = -6, 10
        self.contract.set_position("bob", i_l, -self.infinity, i_u, -self.infinity, 17)
        self.assertEqual(self.contract.positions[("bob", i_l, i_u)].L, 17)
        self.assertEqual(self.contract.L, 17)
        self.assertEqual(self.contract.ticks[i_l].Delta_L, 17)
        self.assertEqual(self.contract.ticks[i_u].Delta_L, 42 - 17)
        i_l, i_u = -20, 0
        self.contract.set_position("alice", i_l, -self.infinity, i_u, -6, 66)
        self.assertEqual(self.contract.L, 17)
        self.assertEqual(self.contract.ticks[i_l].Delta_L, 66)
        self.assertEqual(self.contract.ticks[i_u].Delta_L, -66)

    def test_X_to_Y_no_fees(self):
        i_l, i_u = -50, 50
        self.contract.set_position("alice", i_l, -self.infinity, i_u, -self.infinity, 4321)
        self.assertEqual(self.contract.L, 4321)
        xy = self.contract.X_to_Y_no_fees(10)
        self.assertEqual(xy.x, -10)
        self.assertLess(xy.y, -xy.x)
        xy = self.contract.X_to_Y_no_fees(10)
        self.assertLessEqual(self.contract.balance.y, 1e-4)
        self.assertLess(-xy.x, 10)

    def test_X_to_Y_fees(self):
        i_l, i_u = -30, 30
        self.contract.set_position("alice", i_l, -self.infinity, i_u, -self.infinity, 10**15)
        xy = self.contract.X_to_Y(1)
        self.assertAlmostEqual(xy.y, 1.0 * (1 - 0.3/100))
        self.feegrowth_coherent()

    def test_Y_to_X_fees(self):
        i_l, i_u = -30, 30
        self.contract.set_position("alice", i_l, -self.infinity, i_u, -self.infinity, 10 ** 15)
        xy = self.contract.Y_to_X(2)
        self.assertAlmostEqual(xy.x, 2.0 * (1 - 0.3/100))
        self.contract.set_position("alice", i_l, -self.infinity, i_u, -self.infinity, -10 ** 15)
        self.contract.set_position("alice", i_l, -self.infinity, i_u, -self.infinity, 47)
        i_l, i_u = -37, 10
        self.contract.set_position("bob", i_l, -self.infinity, i_u, -self.infinity, 35)
        xy = self.contract.Y_to_X(100)
        self.feegrowth_coherent()

    def test_fee_accounting(self):
        i_l, i_u = -10, 10
        initial_balance = self.contract.balance
        alice_deposit = self.contract.set_position("alice", i_l, -self.infinity, i_u, -self.infinity, 23456)
        self.feegrowth_coherent()
        user_y_to_x = self.contract.Y_to_X(3.14)
        self.feegrowth_coherent()
        user_x_to_y = self.contract.X_to_Y(3.14)
        self.feegrowth_coherent()
        alice_recovers = self.contract.set_position("alice", i_l, -self.infinity, i_u, i_l, -23456)
        self.feegrowth_coherent()
        accounting = alice_recovers + user_x_to_y + user_y_to_x + alice_deposit

        self.assertAlmostEqual(accounting.x, 0.0)
        self.assertAlmostEqual(accounting.y, 0.0)
        self.assertAlmostEqual(self.contract.balance.x, initial_balance.x)
        self.assertAlmostEqual(self.contract.balance.y, initial_balance.y)


    def test_fee_growth_logic(self):
        i_l, i_u = -20, 20
        initial_balance = self.contract.balance
        alice_deposit = self.contract.set_position("alice", i_l, -self.infinity, i_u, -self.infinity, 23456)
        self.feegrowth_coherent()
        user_y_to_x = self.contract.Y_to_X(3.14)
        self.feegrowth_coherent()
        user_x_to_y = self.contract.X_to_Y(3.14)
        self.feegrowth_coherent()

        bob_deposit = self.contract.set_position("bob", -1, -self.infinity, 1, -self.infinity, 4242)
        self.feegrowth_coherent()
        t1 = self.contract.Y_to_X_no_fees(3.18)
        self.feegrowth_coherent()
        t2 = self.contract.X_to_Y_no_fees(t1.x)
        self.feegrowth_coherent()
        bob_recovers = self.contract.set_position("bob", -1, -self.infinity, 1, -self.infinity, -4242)
        self.feegrowth_coherent()
        bob_total = bob_deposit + bob_recovers

        self.assertAlmostEqual(bob_total.x, 0.0)
        self.assertAlmostEqual(bob_total.y, 0.0)

        bob_deposit = self.contract.set_position("bob", -1, -self.infinity, 1, -self.infinity, 4242)
        t1 = self.contract.X_to_Y_no_fees(3.18)
        t2 = self.contract.Y_to_X_no_fees(t1.y)
        bob_recovers = self.contract.set_position("bob", -1, -self.infinity, 1, -self.infinity, -4242)
        bob_total = bob_deposit + bob_recovers

        self.assertAlmostEqual(bob_total.x, 0.0)
        self.assertAlmostEqual(bob_total.y, 0.0)

        alice_recovers = self.contract.set_position("alice", i_l, -self.infinity, i_u, i_l, -1000)

    # a set of positions creating constant liquidity across a range should behave like uniswap
    def test_equivalent_to_constant_product(self):

        self.contract.set_position("Alice+", 0, -self.infinity, self.infinity, -self.infinity, 321)
        self.contract.set_position("Alice-", -self.infinity, -self.infinity, 0, -self.infinity, 321)

        self.contract.set_position("Alpha", -self.infinity, -self.infinity, -1000, -self.infinity, 4200)
        self.contract.set_position("Bravo", -1000, -self.infinity, -500, -self.infinity, 2100)
        self.contract.set_position("Charlie", -1000, -self.infinity, -750, -self.infinity, 2100)
        self.contract.set_position("Delta", -750, -self.infinity, 0, -self.infinity, 2100)
        self.contract.set_position("Echo", -500, -self.infinity, 0, -self.infinity, 2100)
        self.contract.set_position("Foxtrot", 0, -self.infinity, 666, -self.infinity, 1000)
        self.contract.set_position("Golf", 0, -self.infinity, 333, -self.infinity, 3200)
        self.contract.set_position("Golf", 333, -self.infinity, 900, -self.infinity, 3200)
        self.contract.set_position("Golf", 666, -self.infinity, 900, -self.infinity, 1000)
        self.contract.set_position("Golf", 900, -self.infinity, self.infinity, -self.infinity, 4200)

        k = self.contract.balance.x * self.contract.balance.y
        for trade in [100, -50, 200, -30, 45.3, 12.5, -200, 1000, -800, 300, 531,-1000,-2000,8765]:
            if trade > 0:
                self.contract.X_to_Y_no_fees(trade)
            else:
                self.contract.Y_to_X_no_fees(-trade)
            new_k =  self.contract.balance.x * self.contract.balance.y
            print("i_a = ", self.contract.i_a)
            self.contract.set_position("Alice+", 0, -self.infinity, self.infinity, -self.infinity, -321)
            self.contract.set_position("Alice-", -self.infinity, -self.infinity, 0, -self.infinity, -321)
            self.contract.set_position("Alice+", 0, -self.infinity, self.infinity, -self.infinity, 321)
            self.contract.set_position("Alice-", -self.infinity, -self.infinity, 0, -self.infinity, 321)

            self.assertLess(abs(new_k / k - 1), 1e-6)

    def test_srp_compute(self):
        self.assertEqual(Contract.srp(-self.infinity), 0)
        self.assertEqual(Contract.srp(self.infinity), self.infinity)
        for tick in [-100, -10, 1000, 324, 193, -230, 1, -1, 0, 10, 42, -591]:
            self.assertAlmostEqual(Contract.srp(tick)**2, 1.0001**tick)







if __name__ == '__main__':
    unittest.main()


