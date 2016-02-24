/*
 *  "BSLib", Brainstorm Library.
 *  Copyright (C) 2015-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
using System;

namespace BSLib
{
	public static class ExtMath
	{
		public static int factorial(int n)
		{
			int result = 1;
			for (int k = 2; k <= n; k++) result *= k;
			return result;
		}

		/**
		 * Calculate the Greatest Common Divisor of two numbers.
		 * @param aa
		 * @param bb
		 * @return
		 */
		public static int gcd(int aa, int bb)
		{
			while (0 != bb) {
				int tmp = bb;
				bb = aa % bb;
				aa = tmp;
			}
			//  The greatest common divisor is the largest positive integer.
			return Math.Abs(aa);
		}

		/**
		 * Finds the greatest common divisor (GCD) of integers in the specified part of an array.
		 * @param numbers
		 * Source array of integers.
		 * @param first
		 * Index of the first element in 'numbers' where the method begins calculation of the GCD.
		 * The method calculates the GCD till the last element in the 'numbers'.
		 * @return
		 * The greatest common divisor of integers through the range [first, {last}] in the 'numbers'.
		 */
		public static int gcd(int[] numbers, int first)
		{
			int result = numbers[first];
			for (int i = first + 1; i < numbers.Length; i++) {
				result = gcd(result, numbers[i]);
			}
			return result;
		}

		/**
		 * Calculate Least Common Multiple of two numbers.
		 * @param a
		 * @param b
		 * @return
		 */
		public static int lcm(int a, int b)
		{
			return a * (b / gcd(a, b));
		}

		/**
		 * Calculate Least Common Multiple of numbers array.
		 * @param ia
		 * @return
		 */
		public static int lcm(int[] ia)
		{
			int result = ia[0];
			for (int i = 1; i < ia.Length; i++) {
				result = lcm(result, ia[i]);
			}

			return result;
		}
	}
}