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
        self.assertEqual(a, XY(a.x, a.y))
        c = XY(a.x + b.x, a.y + b.y)
        self.assertEqual(a + b, c)
        c = XY(a.x - b.x, a.y - b.y)
        self.assertEqual(a - b, c)
        c = XY(- b.x, - b.y)
        self.assertEqual(-b, c)
        c = XY(7 * a.x, 7 * a.y)
        self.assertEqual(a * 7, c)

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
        self.subTest(i=0)
        self.assertEqual(Contract.tick(2.71814592682),20000)
        self.subTest(i=1)
        self.assertEqual(Contract.tick(2.71814592681), 19999)
        self.subTest(i=2)
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
        self.assertEqual(contract.ticks[i].i_next, infinity)

    def test_initialize_tick(self):
        i = 0
        i_l = -self.infinity
        self.contract.initialize_tick(i, i_l)
        self.assertEqual(len(self.contract.ticks), 3)
        self.assertEqual(self.contract.ticks[0].i_prev, -self.infinity)
        self.assertEqual(self.contract.ticks[0].i_next, self.infinity)
        self.ticks_is_linked_list(self.contract)

    def test_set_position(self):
        i_l, i_u = 10, 50
        self.contract.set_position("alice", i_l, -self.infinity, i_u, -self.infinity, 42)
        # price should be 1, so i_a = 0 so we should be out of the liquidity region which
        # should be between -infinity and 0
        self.assertEqual(self.contract.L, 0)
        self.assertEqual(self.contract.ticks[i_l].ΔL, 42)
        self.assertEqual(self.contract.ticks[i_u].ΔL, -42)
        i_l, i_u = -6, 10
        self.contract.set_position("bob", i_l, -self.infinity, i_u, -self.infinity, 17)
        self.assertEqual(self.contract.L, 17)
        self.assertEqual(self.contract.ticks[i_l].ΔL, 17)
        self.assertEqual(self.contract.ticks[i_u].ΔL, 42 - 17)
        i_l, i_u = -20, 0
        self.contract.set_position("alice", i_l, -self.infinity, i_u, -6, 66)
        self.assertEqual(self.contract.L, 17)
        self.assertEqual(self.contract.ticks[i_l].ΔL, 66)
        self.assertEqual(self.contract.ticks[i_u].ΔL, -66)

if __name__ == '__main__':
    unittest.main()


