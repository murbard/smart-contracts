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

    def test_fee_accounting(self):
        i_l, i_u = -10, 10
        initial_balance = self.contract.balance
        alice_deposit = self.contract.set_position("alice", i_l, -self.infinity, i_u, -self.infinity, 23456)
        user_y_to_x = self.contract.Y_to_X(3.14)
        user_x_to_y = self.contract.X_to_Y(3.14)
        alice_recovers = self.contract.set_position("alice", i_l, -self.infinity, i_u, i_l, -23456)

        accounting = alice_recovers + user_x_to_y + user_y_to_x + alice_deposit

        self.assertAlmostEqual(accounting.x, 0.0)
        self.assertAlmostEqual(accounting.y, 0.0)
        self.assertAlmostEqual(self.contract.balance.x, initial_balance.x)
        self.assertAlmostEqual(self.contract.balance.y, initial_balance.y)

    def test_fee_growth_logic(self):
        i_l, i_u = -20, 20
        initial_balance = self.contract.balance
        alice_deposit = self.contract.set_position("alice", i_l, -self.infinity, i_u, -self.infinity, 23456)
        user_y_to_x = self.contract.Y_to_X(3.14)
        user_x_to_y = self.contract.X_to_Y(3.14)

        bob_deposit = self.contract.set_position("bob", -1, -self.infinity, 1, -self.infinity, 4242)
        t1 = self.contract.Y_to_X_no_fees(3.18)
        t2 = self.contract.X_to_Y_no_fees(t1.x)
        bob_recovers = self.contract.set_position("bob", -1, -self.infinity, 1, -self.infinity, -4242)
        bob_total = bob_deposit + bob_recovers

        self.assertAlmostEqual(bob_total.x, 0.0)
        self.assertAlmostEqual(bob_total.y, 0.0)
        alice_recovers = self.contract.set_position("alice", i_l, -self.infinity, i_u, i_l, -1000)

if __name__ == '__main__':
    unittest.main()


