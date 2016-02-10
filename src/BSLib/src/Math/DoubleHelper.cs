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
	/**
	 * A class that tests for equality, less than, and greater than with two doubles.
	 * (Translated from Java).
	 *
	 * @see http://stackoverflow.com/questions/356807
	 */
	public static class DoubleHelper
	{
		private const double EPSILON = 0.00001;

		/**
		 * Returns true if two doubles are considered equal. Tests if the absolute
		 * difference between two doubles has a difference less then .00001. This
		 * should be fine when comparing prices, because prices have a precision of
		 * .001.
		 *
		 * @param a double to compare.
		 * @param b double to compare.
		 * @return true true if two doubles are considered equal.
		 */
		public static bool equals(double a, double b)
		{
			return a == b ? true : Math.Abs(a - b) < EPSILON;
		}

		/**
		 * Returns true if two doubles are considered equal. Tests if the absolute
		 * difference between the two doubles has a difference less then a given
		 * double (epsilon). Determining the given epsilon is highly dependant on
		 * the precision of the doubles that are being compared.
		 *
		 * @param a double to compare.
		 * @param b double to compare
		 * @param epsilon double which is compared to the absolute difference of two
		 * doubles to determine if they are equal.
		 * @return true if a is considered equal to b.
		 */
		public static bool equals(double a, double b, double epsilon)
		{
			return a == b ? true : Math.Abs(a - b) < epsilon;
		}

		/**
		 * Returns true if the first double is considered greater than the second
		 * double. Test if the difference of first minus second is greater then
		 * .00001. This should be fine when comparing prices, because prices have a
		 * precision of .001.
		 *
		 * @param a first double
		 * @param b second double
		 * @return true if the first double is considered greater than the second
		 * double
		 */
		public static bool greaterThan(double a, double b)
		{
			return greaterThan(a, b, EPSILON);
		}

		/**
		 * Returns true if the first double is considered greater than the second
		 * double. Test if the difference of first minus second is greater then a
		 * given double (epsilon). Determining the given epsilon is highly dependant
		 * on the precision of the doubles that are being compared.
		 *
		 * @param a first double
		 * @param b second double
		 * @return true if the first double is considered greater than the second
		 * double
		 */
		public static bool greaterThan(double a, double b, double epsilon)
		{
			return a - b > epsilon;
		}

		/**
		 * Returns true if the first double is considered less than the second
		 * double. Test if the difference of second minus first is greater then
		 * .00001. This should be fine when comparing prices, because prices have a
		 * precision of .001.
		 *
		 * @param a first double
		 * @param b second double
		 * @return true if the first double is considered less than the second
		 * double
		 */
		public static bool lessThan(double a, double b)
		{
			return lessThan(a, b, EPSILON);
		}

		/**
		 * Returns true if the first double is considered less than the second
		 * double. Test if the difference of second minus first is greater then a
		 * given double (epsilon). Determining the given epsilon is highly dependant
		 * on the precision of the doubles that are being compared.
		 *
		 * @param a first double
		 * @param b second double
		 * @return true if the first double is considered less than the second
		 * double
		 */
		public static bool lessThan(double a, double b, double epsilon)
		{
			return b - a > epsilon;
		}
	}
}