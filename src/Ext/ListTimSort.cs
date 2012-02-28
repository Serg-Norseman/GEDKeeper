#region Licence

/*
 * Copyright (C) 2008 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#endregion

#region Notes

//------------------------------------------------------------------------------
// Java implementation:
//
// A stable, adaptive, iterative mergesort that requires far fewer than
// n lg(n) comparisons when running on partially sorted arrays, while
// offering performance comparable to a traditional mergesort when run
// on random arrays.  Like all proper mergesorts, this sort is stable and
// runs O(n log n) time (worst case).  In the worst case, this sort requires
// temporary storage space for n/2 object references; in the best case,
// it requires only a small constant amount of space.
// 
// This implementation was adapted from Tim Peters's list sort for
// Python, which is described in detail here:
// http://svn.python.org/projects/python/trunk/Objects/listsort.txt
// 
// Tim's C code may be found here:
// http://svn.python.org/projects/python/trunk/Objects/listobject.c
// 
// The underlying techniques are described in this paper (and may have
// even earlier origins):
// 
// "Optimistic Sorting and Information Theoretic Complexity"
// Peter McIlroy
// SODA (Fourth Annual ACM-SIAM Symposium on Discrete Algorithms),
// pp 467-474, Austin, Texas, 25-27 January 1993.
// 
// While the API to this class consists solely of static methods, it is
// (privately) instantiable; a TimSort instance holds the state of an ongoing
// sort, assuming the input array is large enough to warrant the full-blown
// TimSort. Small arrays are sorted in place, using a binary insertion sort.
// 
// author: Josh Bloch
//------------------------------------------------------------------------------
// C# implementation:
//
// This implementation was adapted from Josh Bloch array sort for Java, 
// which has been found here:
// http://gee.cs.oswego.edu/cgi-bin/viewcvs.cgi/jsr166/src/main/java/util/TimSort.java?view=co
// 
// author: Milosz Krajewski <mkrajewski@sf.net>
//------------------------------------------------------------------------------

#endregion

using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace TimSort
{
	#region class ListTimSort<T>

	/// <summary>TimSort implementation for List.</summary>
	/// <typeparam name="T">Type of item.</typeparam>
	internal class ListTimSort<T>
	{
		/// <summary>
		/// This is the minimum sized sequence that will be merged.  Shorter
		/// sequences will be lengthened by calling binarySort.  If the entire
		/// array is less than this length, no merges will be performed.
		/// This constant should be a power of two.  It was 64 in Tim Peter's C
		/// implementation, but 32 was empirically determined to work better in
		/// this implementation.  In the unlikely event that you set this constant
		/// to be a number that's not a power of two, you'll need to change the
		/// minRunLength computation.
		/// If you decrease this constant, you must change the stackLen
		/// computation in the TimSort constructor, or you risk an
		/// ArrayOutOfBounds exception.  See listsort.txt for a discussion
		/// of the minimum stack length required as a function of the length
		/// of the array being sorted and the minimum merge sequence length.
		/// </summary>
		private const int MIN_MERGE = 32;

		/// <summary>
		/// When we get into galloping mode, we stay there until both runs win less
		/// often than MIN_GALLOP consecutive times.
		/// </summary>
		private const int MIN_GALLOP = 7;

		/// <summary>
		/// Maximum initial size of tmp array, which is used for merging.  The array
		/// can grow to accommodate demand.
		/// Unlike Tim's original C version, we do not allocate this much storage
		/// when sorting smaller arrays.  This change was required for performance.
		/// </summary>
		private const int INITIAL_TMP_STORAGE_LENGTH = 256;

		/// <summary>The array being sorted.</summary>
		private readonly IList<T> m_Array;

		/**
		 * The comparator for this sort.
		 */
		private readonly Comparison<T> m_Comparer;

		/// <summary>
		/// This controls when we get *into* galloping mode.  It is initialized
		/// to MIN_GALLOP.  The mergeLo and mergeHi methods nudge it higher for
		/// random data, and lower for highly structured data.
		/// </summary>
		private int m_MinGallop = MIN_GALLOP;

		/// <summary>
		/// Temp storage for merges.
		/// </summary>
		private T[] m_MergeBuffer;

		/// <summary>
		/// A stack of pending runs yet to be merged.  Run i starts at
		/// address base[i] and extends for len[i] elements.  It's always
		/// true (so long as the indices are in bounds) that:
		/// <c>runBase[i] + runLen[i] == runBase[i + 1]</c>
		/// so we could cut the storage for this, but it's a minor amount,
		/// and keeping all the info explicit simplifies the code.
		/// </summary>
		private int m_StackSize; // = 0; // Number of pending runs on stack
		private int[] m_RunBase;
		private int[] m_RunLength;

		/// <summary>
		/// Prevents a default instance of the <see cref="TimSort&lt;T&gt;"/> class from being created.
		/// Creates a TimSort instance to maintain the state of an ongoing sort.
		/// </summary>
		/// <param name="array">The array to be sorted.</param>
		/// <param name="comparer">The comparator to determine the order of the sort.</param>
		private ListTimSort(IList<T> array, Comparison<T> comparer)
		{
			m_Array = array;
			m_Comparer = comparer;

			// Allocate temp storage (which may be increased later if necessary)
			int arrayLength = array.Count;
			int mergeBufferLength =
				arrayLength < 2 * INITIAL_TMP_STORAGE_LENGTH
				? arrayLength >> 1
				: INITIAL_TMP_STORAGE_LENGTH;
			m_MergeBuffer = new T[mergeBufferLength];

			// Allocate runs-to-be-merged stack (which cannot be expanded).  The
			// stack length requirements are described in listsort.txt.  The C
			// version always uses the same stack length (85), but this was
			// measured to be too expensive when sorting "mid-sized" arrays (e.g.,
			// 100 elements) in Java.  Therefore, we use smaller (but sufficiently
			// large) stack lengths for smaller arrays.  The "magic numbers" in the
			// computation below must be changed if MIN_MERGE is decreased.  See
			// the MIN_MERGE declaration above for more information.
			int stackLength =
				arrayLength < 120 ? 5 :
				arrayLength < 1542 ? 10 :
				arrayLength < 119151 ? 19 :
				40;
			m_RunBase = new int[stackLength];
			m_RunLength = new int[stackLength];
		}

		/// <summary>Sorts the specified array.</summary>
		/// <param name="array">Array to be sorted.</param>
		/// <param name="comparer">Comparer.</param>
		public static void Sort(IList<T> array, Comparison<T> comparer)
		{
			Sort(array, 0, array.Count, comparer);
		}

		/// <summary>Sorts the specified array.</summary>
		/// <param name="a">Array to be sorted.</param>
		/// <param name="lo">the index of the first element in the range to be sorted.</param>
		/// <param name="hi">the index after the last element in the range to be sorted.</param>
		/// <param name="comparer">The comparator to determine the order of the sort.</param>
		public static void Sort(IList<T> a, int lo, int hi, Comparison<T> c)
		{
			CheckRange(a.Count, lo, hi);

			int width = hi - lo;
			if (width < 2) return; // Arrays of size 0 and 1 are always sorted

			// If array is small, do a "mini-TimSort" with no merges
			if (width < MIN_MERGE)
			{
				int initRunLength = CountRunAndMakeAscending(a, lo, hi, c);
				BinarySort(a, lo, hi, lo + initRunLength, c);
				return;
			}

			// March over the array once, left to right, finding natural runs,
			// extending short natural runs to minRun elements, and merging runs
			// to maintain stack invariant.
			var sorter = new ListTimSort<T>(a, c);
			int minRun = GetMinimumRunLength(width);
			do
			{
				// Identify next run
				int runLen = CountRunAndMakeAscending(a, lo, hi, c);

				// If run is short, extend to min(minRun, nRemaining)
				if (runLen < minRun)
				{
					int force = width <= minRun ? width : minRun;
					BinarySort(a, lo, lo + force, lo + runLen, c);
					runLen = force;
				}

				// Push run onto pending-run stack, and maybe merge
				sorter.PushRun(lo, runLen);
				sorter.MergeCollapse();

				// Advance to find next run
				lo += runLen;
				width -= runLen;
			} while (width != 0);

			// Merge all remaining runs to complete sort
			Debug.Assert(lo == hi);
			sorter.MergeForceCollapse();
			Debug.Assert(sorter.m_StackSize == 1);
		}

		/// <summary>
		/// Sorts the specified portion of the specified array using a binary insertion sort. This is the best method for 
		/// sorting small numbers of elements. It requires O(n log n) compares, but O(n^2) data movement (worst case).
		/// If the initial part of the specified range is already sorted, this method can take advantage of it: the method 
		/// assumes that the elements from index <c>lo</c>, inclusive, to <c>start</c>, exclusive are already sorted.
		/// </summary>
		/// <param name="a">the array in which a range is to be sorted.</param>
		/// <param name="lo">the index of the first element in the range to be sorted.</param>
		/// <param name="hi">the index after the last element in the range to be sorted.</param>
		/// <param name="start">start the index of the first element in the range that is not already known to be sorted 
		/// (<c><![CDATA[lo <= start <= hi]]></c>)</param>
		/// <param name="c">The comparator to used for the sort.</param>
		private static void BinarySort(IList<T> a, int lo, int hi, int start, Comparison<T> c)
		{
			Debug.Assert(lo <= start && start <= hi);

			if (start == lo) start++;

			for (/* nothing */; start < hi; start++)
			{
				T pivot = a[start];

				// Set left (and right) to the index where a[start] (pivot) belongs
				int left = lo;
				int right = start;
				Debug.Assert(left <= right);

				/*
				 * Invariants:
				 *   pivot >= all in [lo, left).
				 *   pivot <  all in [right, start).
				 */
				while (left < right)
				{
					int mid = (left + right) >> 1;
					if (c(pivot, a[mid]) < 0)
					{
						right = mid;
					}
					else
					{
						left = mid + 1;
					}
				}
				Debug.Assert(left == right);

				// The invariants still hold: pivot >= all in [lo, left) and
				// pivot < all in [left, start), so pivot belongs at left.  Note
				// that if there are elements equal to pivot, left points to the
				// first slot after them -- that's why this sort is stable.
				// Slide elements over to make room to make room for pivot.

				int n = start - left; // The number of elements to move

				// switch is just an optimization for copyRange in default case
				switch (n)
				{
					case 2:
						a[left + 2] = a[left + 1];
						a[left + 1] = a[left];
						break;
					case 1:
						a[left + 1] = a[left];
						break;
					case 0:
						break;
					default:
						CopyRange(a, left, a, left + 1, n);
						break;
				}
				a[left] = pivot;
			}
		}

		/// <summary>
		/// Returns the length of the run beginning at the specified position in
		/// the specified array and reverses the run if it is descending (ensuring
		/// that the run will always be ascending when the method returns).
		/// A run is the longest ascending sequence with: <c><![CDATA[a[lo] <= a[lo + 1] <= a[lo + 2] <= ...]]></c>
		/// or the longest descending sequence with: <c><![CDATA[a[lo] >  a[lo + 1] >  a[lo + 2] >  ...]]></c>
		/// For its intended use in a stable mergesort, the strictness of the
		/// definition of "descending" is needed so that the call can safely
		/// reverse a descending sequence without violating stability.
		/// </summary>
		/// <param name="a">the array in which a run is to be counted and possibly reversed.</param>
		/// <param name="lo">index of the first element in the run.</param>
		/// <param name="hi">index after the last element that may be contained in the run. It is required 
		/// that <c><![CDATA[lo < hi]]></c>.</param>
		/// <param name="c">the comparator to used for the sort.</param>
		/// <returns>the length of the run beginning at the specified position in the specified array</returns>
		private static int CountRunAndMakeAscending(IList<T> a, int lo, int hi, Comparison<T> c)
		{
			Debug.Assert(lo < hi);
			int runHi = lo + 1;
			if (runHi == hi) return 1;

			// Find end of run, and reverse range if descending
			if (c(a[runHi++], a[lo]) < 0)
			{
				// Descending
				while (runHi < hi && c(a[runHi], a[runHi - 1]) < 0) runHi++;
				ReverseRange(a, lo, runHi);
			}
			else
			{
				// Ascending
				while (runHi < hi && c(a[runHi], a[runHi - 1]) >= 0) runHi++;
			}

			return runHi - lo;
		}

		/// <summary>Copies the range from one array to another.</summary>
		/// <param name="src">The source array.</param>
		/// <param name="srcIndex">Starting index in source array.</param>
		/// <param name="dst">The destination array.</param>
		/// <param name="dstIndex">Starting index in destination array.</param>
		/// <param name="length">Number of elements to be copied.</param>
		private static void CopyRange(IList<T> src, int srcIndex, IList<T> dst, int dstIndex, int length)
		{
			if (length <= 0) return;

			if (dstIndex - srcIndex > 0) // copy forward => use reverse order
			{
				srcIndex += length - 1;
				dstIndex += length - 1;
				while (length-- > 0) dst[dstIndex--] = src[srcIndex--];
			}
			else // copy to different array or backwards => use natural order
			{
				while (length-- > 0) dst[dstIndex++] = src[srcIndex++];
			}
		}

		/// <summary>Copies the range from one array to another.</summary>
		/// <param name="src">The source array.</param>
		/// <param name="srcIndex">Starting index in source array.</param>
		/// <param name="dst">The destination array.</param>
		/// <param name="dstIndex">Starting index in destination array.</param>
		/// <param name="length">Number of elements to be copied.</param>
		private static void CopyRange(T[] src, int srcIndex, IList<T> dst, int dstIndex, int length)
		{
			while (length-- > 0) dst[dstIndex++] = src[srcIndex++];
		}

		/// <summary>Copies the range from one array to another.</summary>
		/// <param name="src">The source array.</param>
		/// <param name="srcIndex">Starting index in source array.</param>
		/// <param name="dst">The destination array.</param>
		/// <param name="dstIndex">Starting index in destination array.</param>
		/// <param name="length">Number of elements to be copied.</param>
		private static void CopyRange(IList<T> src, int srcIndex, T[] dst, int dstIndex, int length)
		{
			while (length-- > 0) dst[dstIndex++] = src[srcIndex++];
		}

		/// <summary>Reverse the specified range of the specified array.</summary>
		/// <param name="array">the array in which a range is to be reversed.</param>
		/// <param name="lo">the index of the first element in the range to be reversed.</param>
		/// <param name="hi">the index after the last element in the range to be reversed.</param>
		private static void ReverseRange(IList<T> array, int lo, int hi)
		{
			hi--;
			while (lo < hi)
			{
				T t = array[lo];
				array[lo++] = array[hi];
				array[hi--] = t;
			}
		}

		/// <summary>
		/// Returns the minimum acceptable run length for an array of the specified length. Natural runs shorter than this 
		/// will be extended with binarySort.
		/// Roughly speaking, the computation is:
		/// If <c>n &lt; MIN_MERGE</c>, return n (it's too small to bother with fancy stuff).
		/// Else if n is an exact power of 2, return <c>MIN_MERGE/2</c>.
		/// Else return an int k, <c>MIN_MERGE/2 &lt;= k &lt;= MIN_MERGE</c>, such that <c>n/k</c> is close to, but strictly 
		/// less than, an exact power of 2. For the rationale, see listsort.txt.
		/// </summary>
		/// <param name="n">the length of the array to be sorted.</param>
		/// <returns>the length of the minimum run to be merged.</returns>
		private static int GetMinimumRunLength(int n)
		{
			Debug.Assert(n >= 0);
			int r = 0; // Becomes 1 if any 1 bits are shifted off
			while (n >= MIN_MERGE)
			{
				r |= (n & 1);
				n >>= 1;
			}
			return n + r;
		}

		/// <summary>
		/// Pushes the specified run onto the pending-run stack.
		/// </summary>
		/// <param name="runBase">index of the first element in the run.</param>
		/// <param name="runLength">the number of elements in the run.</param>
		private void PushRun(int runBase, int runLength)
		{
			m_RunBase[m_StackSize] = runBase;
			m_RunLength[m_StackSize] = runLength;
			m_StackSize++;
		}

		/// <summary>
		/// Examines the stack of runs waiting to be merged and merges adjacent runs until the stack invariants are
		/// reestablished: 
		/// <c><![CDATA[1. runLen[i - 3] > runLen[i - 2] + runLen[i - 1] ]]></c> and 
		/// <c><![CDATA[2. runLen[i - 2] > runLen[i - 1] ]]></c>
		/// This method is called each time a new run is pushed onto the stack,
		/// so the invariants are guaranteed to hold for i &lt; stackSize upon
		/// entry to the method.
		/// </summary>
		private void MergeCollapse()
		{
			while (m_StackSize > 1)
			{
				int n = m_StackSize - 2;

				if (n > 0 && m_RunLength[n - 1] <= m_RunLength[n] + m_RunLength[n + 1])
				{
					if (m_RunLength[n - 1] < m_RunLength[n + 1]) n--;
					MergeAt(n);
				}
				else if (m_RunLength[n] <= m_RunLength[n + 1])
				{
					MergeAt(n);
				}
				else
				{
					break; // Invariant is established
				}
			}
		}

		/// <summary>
		/// Merges all runs on the stack until only one remains.  This method is called once, to complete the sort.
		/// </summary>
		private void MergeForceCollapse()
		{
			while (m_StackSize > 1)
			{
				int n = m_StackSize - 2;
				if (n > 0 && m_RunLength[n - 1] < m_RunLength[n + 1]) n--;
				MergeAt(n);
			}
		}

		/// <summary>
		/// Merges the two runs at stack indices i and i+1.  Run i must be the penultimate or antepenultimate run on the stack. 
		/// In other words, i must be equal to stackSize-2 or stackSize-3.
		/// </summary>
		/// <param name="runIndex">stack index of the first of the two runs to merge.</param>
		private void MergeAt(int runIndex)
		{
			Debug.Assert(m_StackSize >= 2);
			Debug.Assert(runIndex >= 0);
			Debug.Assert(runIndex == m_StackSize - 2 || runIndex == m_StackSize - 3);

			int base1 = m_RunBase[runIndex];
			int len1 = m_RunLength[runIndex];
			int base2 = m_RunBase[runIndex + 1];
			int len2 = m_RunLength[runIndex + 1];
			Debug.Assert(len1 > 0 && len2 > 0);
			Debug.Assert(base1 + len1 == base2);

			// Record the length of the combined runs; if i is the 3rd-last
			// run now, also slide over the last run (which isn't involved
			// in this merge). The current run (i+1) goes away in any case.
			m_RunLength[runIndex] = len1 + len2;
			if (runIndex == m_StackSize - 3)
			{
				m_RunBase[runIndex + 1] = m_RunBase[runIndex + 2];
				m_RunLength[runIndex + 1] = m_RunLength[runIndex + 2];
			}
			m_StackSize--;

			// Find where the first element of run2 goes in run1. Prior elements
			// in run1 can be ignored (because they're already in place).
			int k = GallopRight(m_Array[base2], m_Array, base1, len1, 0, m_Comparer);
			Debug.Assert(k >= 0);
			base1 += k;
			len1 -= k;
			if (len1 == 0) return;

			// Find where the last element of run1 goes in run2. Subsequent elements
			// in run2 can be ignored (because they're already in place).
			len2 = GallopLeft(m_Array[base1 + len1 - 1], m_Array, base2, len2, len2 - 1, m_Comparer);
			Debug.Assert(len2 >= 0);
			if (len2 == 0) return;

			// Merge remaining runs, using tmp array with min(len1, len2) elements
			if (len1 <= len2)
				MergeLo(base1, len1, base2, len2);
			else
				MergeHi(base1, len1, base2, len2);
		}

		/// <summary>
		/// Locates the position at which to insert the specified key into the
		/// specified sorted range; if the range contains an element equal to key,
		/// returns the index of the leftmost equal element.
		/// </summary>
		/// <param name="key">the key whose insertion point to search for.</param>
		/// <param name="array">the array in which to search.</param>
		/// <param name="base">the index of the first element in the range.</param>
		/// <param name="length">the length of the range; must be &gt; 0.</param>
		/// <param name="hint">the index at which to begin the search, 0 &lt;= hint &lt; n. The closer hint is to the result, 
		/// the faster this method will run.</param>
		/// <param name="comparer">the comparator used to order the range, and to search.</param>
		/// <returns>the int k,  0 &lt;= k &lt;= n such that a[b + k - 1] &lt; key &lt;= a[b + k], pretending that a[b - 1] 
		/// is minus infinity and a[b + n] is infinity. In other words, key belongs at index b + k; or in other words, the 
		/// first k elements of a should precede key, and the last n - k should follow it.</returns>
		private static int GallopLeft(T key, IList<T> array, int lo, int length, int hint, Comparison<T> comparer)
		{
			Debug.Assert(length > 0 && hint >= 0 && hint < length);
			int lastOfs = 0;
			int ofs = 1;

			if (comparer(key, array[lo + hint]) > 0)
			{
				// Gallop right until a[base+hint+lastOfs] < key <= a[base+hint+ofs]
				int maxOfs = length - hint;
				while (ofs < maxOfs && comparer(key, array[lo + hint + ofs]) > 0)
				{
					lastOfs = ofs;
					ofs = (ofs << 1) + 1;
					if (ofs <= 0)   // int overflow
						ofs = maxOfs;
				}
				if (ofs > maxOfs)
					ofs = maxOfs;

				// Make offsets relative to base
				lastOfs += hint;
				ofs += hint;
			}
			else // if (key <= a[base + hint])
			{
				// Gallop left until a[base+hint-ofs] < key <= a[base+hint-lastOfs]
				int maxOfs = hint + 1;
				while (ofs < maxOfs && comparer(key, array[lo + hint - ofs]) <= 0)
				{
					lastOfs = ofs;
					ofs = (ofs << 1) + 1;
					if (ofs <= 0) // int overflow
						ofs = maxOfs;
				}
				if (ofs > maxOfs)
					ofs = maxOfs;

				// Make offsets relative to base
				int tmp = lastOfs;
				lastOfs = hint - ofs;
				ofs = hint - tmp;
			}
			Debug.Assert(-1 <= lastOfs && lastOfs < ofs && ofs <= length);

			// Now a[base+lastOfs] < key <= a[base+ofs], so key belongs somewhere
			// to the right of lastOfs but no farther right than ofs.  Do a binary
			// search, with invariant a[base + lastOfs - 1] < key <= a[base + ofs].
			lastOfs++;
			while (lastOfs < ofs)
			{
				int m = lastOfs + ((ofs - lastOfs) >> 1);

				if (comparer(key, array[lo + m]) > 0)
					lastOfs = m + 1; // a[base + m] < key
				else
					ofs = m; // key <= a[base + m]
			}
			Debug.Assert(lastOfs == ofs); // so a[base + ofs - 1] < key <= a[base + ofs]
			return ofs;
		}

		/// <summary>
		/// Like gallopLeft, except that if the range contains an element equal to
		/// key, gallopRight returns the index after the rightmost equal element.
		/// </summary>
		/// <param name="key">the key whose insertion point to search for.</param>
		/// <param name="array">the array in which to search.</param>
		/// <param name="lo">the index of the first element in the range.</param>
		/// <param name="length">the length of the range; must be &gt; 0.</param>
		/// <param name="hint">the index at which to begin the search, 0 &lt;= hint &lt; n. The closer hint is to the result, 
		/// the faster this method will run.</param>
		/// <param name="comparer">the comparator used to order the range, and to search.</param>
		/// <returns>int k, that 0 &lt;= k &lt;= n such that a[b + k - 1] &lt;= key &lt; a[b + k]</returns>
		private static int GallopRight(T key, IList<T> array, int lo, int length, int hint, Comparison<T> comparer)
		{
			Debug.Assert(length > 0 && hint >= 0 && hint < length);

			int ofs = 1;
			int lastOfs = 0;
			if (comparer(key, array[lo + hint]) < 0)
			{
				// Gallop left until a[b+hint - ofs] <= key < a[b+hint - lastOfs]
				int maxOfs = hint + 1;
				while (ofs < maxOfs && comparer(key, array[lo + hint - ofs]) < 0)
				{
					lastOfs = ofs;
					ofs = (ofs << 1) + 1;
					if (ofs <= 0)   // int overflow
						ofs = maxOfs;
				}
				if (ofs > maxOfs)
					ofs = maxOfs;

				// Make offsets relative to b
				int tmp = lastOfs;
				lastOfs = hint - ofs;
				ofs = hint - tmp;
			}
			else
			{
				// a[b + hint] <= key
				// Gallop right until a[b+hint + lastOfs] <= key < a[b+hint + ofs]
				int maxOfs = length - hint;
				while (ofs < maxOfs && comparer(key, array[lo + hint + ofs]) >= 0)
				{
					lastOfs = ofs;
					ofs = (ofs << 1) + 1;
					if (ofs <= 0)   // int overflow
						ofs = maxOfs;
				}
				if (ofs > maxOfs)
					ofs = maxOfs;

				// Make offsets relative to b
				lastOfs += hint;
				ofs += hint;
			}
			Debug.Assert(-1 <= lastOfs && lastOfs < ofs && ofs <= length);

			// Now a[b + lastOfs] <= key < a[b + ofs], so key belongs somewhere to
			// the right of lastOfs but no farther right than ofs.  Do a binary
			// search, with invariant a[b + lastOfs - 1] <= key < a[b + ofs].
			lastOfs++;
			while (lastOfs < ofs)
			{
				int m = lastOfs + ((ofs - lastOfs) >> 1);

				if (comparer(key, array[lo + m]) < 0)
					ofs = m; // key < a[b + m]
				else
					lastOfs = m + 1; // a[b + m] <= key
			}

			Debug.Assert(lastOfs == ofs); // so a[b + ofs - 1] <= key < a[b + ofs]
			return ofs;
		}

		/// <summary>
		/// Merges two adjacent runs in place, in a stable fashion. The first element of the first run must be greater than 
		/// the first element of the second run (a[base1] &gt; a[base2]), and the last element of the first run 
		/// (a[base1 + len1-1]) must be greater than all elements of the second run.
		/// For performance, this method should be called only when len1 &lt;= len2; its twin, mergeHi should be called if 
		/// len1 &gt;= len2. (Either method may be called if len1 == len2.)
		/// </summary>
		/// <param name="base1">index of first element in first run to be merged.</param>
		/// <param name="len1">length of first run to be merged (must be &gt; 0).</param>
		/// <param name="base2">index of first element in second run to be merged (must be aBase + aLen).</param>
		/// <param name="len2">length of second run to be merged (must be &gt; 0).</param>
		private void MergeLo(int base1, int len1, int base2, int len2)
		{
			Debug.Assert(len1 > 0 && len2 > 0 && base1 + len1 == base2);

			// Copy first run into temp array
			var array = m_Array; // For performance
			var mergeBuffer = EnsureCapacity(len1);
			CopyRange(array, base1, mergeBuffer, 0, len1);

			int cursor1 = 0;       // Indexes into tmp array
			int cursor2 = base2;   // Indexes int a
			int dest = base1;      // Indexes int a

			// Move first element of second run and deal with degenerate cases
			array[dest++] = array[cursor2++];
			if (--len2 == 0)
			{
				CopyRange(mergeBuffer, cursor1, array, dest, len1);
				return;
			}
			if (len1 == 1)
			{
				CopyRange(array, cursor2, array, dest, len2);
				array[dest + len2] = mergeBuffer[cursor1]; // Last elt of run 1 to end of merge
				return;
			}

			var c = m_Comparer;  // Use local variables for performance
			int minGallop = m_MinGallop;

			while (true)
			{
				int count1 = 0; // Number of times in a row that first run won
				int count2 = 0; // Number of times in a row that second run won

				/*
				 * Do the straightforward thing until (if ever) one run starts
				 * winning consistently.
				 */
				do
				{
					Debug.Assert(len1 > 1 && len2 > 0);
					if (c(array[cursor2], mergeBuffer[cursor1]) < 0)
					{
						array[dest++] = array[cursor2++];
						count2++;
						count1 = 0;
						if (--len2 == 0)
							goto break_outer;
					}
					else
					{
						array[dest++] = mergeBuffer[cursor1++];
						count1++;
						count2 = 0;
						if (--len1 == 1)
							goto break_outer;
					}
				} while ((count1 | count2) < minGallop);

				// One run is winning so consistently that galloping may be a
				// huge win. So try that, and continue galloping until (if ever)
				// neither run appears to be winning consistently anymore.
				do
				{
					Debug.Assert(len1 > 1 && len2 > 0);
					count1 = GallopRight(array[cursor2], mergeBuffer, cursor1, len1, 0, c);
					if (count1 != 0)
					{
						CopyRange(mergeBuffer, cursor1, array, dest, count1);
						dest += count1;
						cursor1 += count1;
						len1 -= count1;
						if (len1 <= 1) // len1 == 1 || len1 == 0
							goto break_outer;
					}
					array[dest++] = array[cursor2++];
					if (--len2 == 0)
						goto break_outer;

					count2 = GallopLeft(mergeBuffer[cursor1], array, cursor2, len2, 0, c);
					if (count2 != 0)
					{
						CopyRange(array, cursor2, array, dest, count2);
						dest += count2;
						cursor2 += count2;
						len2 -= count2;
						if (len2 == 0)
							goto break_outer;
					}
					array[dest++] = mergeBuffer[cursor1++];
					if (--len1 == 1)
						goto break_outer;
					minGallop--;
				} while (count1 >= MIN_GALLOP | count2 >= MIN_GALLOP);

				if (minGallop < 0)
					minGallop = 0;
				minGallop += 2;  // Penalize for leaving gallop mode
			}  // End of "outer" loop

			break_outer: // goto me! ;)

			m_MinGallop = minGallop < 1 ? 1 : minGallop;  // Write back to field

			if (len1 == 1)
			{
				Debug.Assert(len2 > 0);
				CopyRange(array, cursor2, array, dest, len2);
				array[dest + len2] = mergeBuffer[cursor1]; //  Last elt of run 1 to end of merge
			}
			else if (len1 == 0)
			{
				throw new ArgumentException("Comparison method violates its general contract!");
			}
			else
			{
				Debug.Assert(len2 == 0);
				Debug.Assert(len1 > 1);
				CopyRange(mergeBuffer, cursor1, array, dest, len1);
			}
		}

		/// <summary>
		/// Like mergeLo, except that this method should be called only if
		/// len1 &gt;= len2; mergeLo should be called if len1 &lt;= len2. (Either method may be called if len1 == len2.)
		/// </summary>
		/// <param name="base1">index of first element in first run to be merged.</param>
		/// <param name="len1">length of first run to be merged (must be &gt; 0).</param>
		/// <param name="base2">index of first element in second run to be merged (must be aBase + aLen).</param>
		/// <param name="len2">length of second run to be merged (must be &gt; 0).</param>
		private void MergeHi(int base1, int len1, int base2, int len2)
		{
			Debug.Assert(len1 > 0 && len2 > 0 && base1 + len1 == base2);

			// Copy second run into temp array
			var a = m_Array; // For performance
			var tmp = EnsureCapacity(len2);
			CopyRange(a, base2, tmp, 0, len2);

			int cursor1 = base1 + len1 - 1;  // Indexes into a
			int cursor2 = len2 - 1;          // Indexes into tmp array
			int dest = base2 + len2 - 1;     // Indexes into a

			// Move last element of first run and deal with degenerate cases
			a[dest--] = a[cursor1--];
			if (--len1 == 0)
			{
				CopyRange(tmp, 0, a, dest - (len2 - 1), len2);
				return;
			}
			if (len2 == 1)
			{
				dest -= len1;
				cursor1 -= len1;
				CopyRange(a, cursor1 + 1, a, dest + 1, len1);
				a[dest] = tmp[cursor2];
				return;
			}

			var c = m_Comparer;  // Use local variables for performance
			int minGallop = m_MinGallop;

			while (true)
			{
				int count1 = 0; // Number of times in a row that first run won
				int count2 = 0; // Number of times in a row that second run won

				// Do the straightforward thing until (if ever) one run appears to win consistently.
				do
				{
					Debug.Assert(len1 > 0 && len2 > 1);
					if (c(tmp[cursor2], a[cursor1]) < 0)
					{
						a[dest--] = a[cursor1--];
						count1++;
						count2 = 0;
						if (--len1 == 0)
							goto break_outer;
					}
					else
					{
						a[dest--] = tmp[cursor2--];
						count2++;
						count1 = 0;
						if (--len2 == 1)
							goto break_outer;
					}
				} while ((count1 | count2) < minGallop);

				// One run is winning so consistently that galloping may be a
				// huge win. So try that, and continue galloping until (if ever)
				// neither run appears to be winning consistently anymore.
				do
				{
					Debug.Assert(len1 > 0 && len2 > 1);
					count1 = len1 - GallopRight(tmp[cursor2], a, base1, len1, len1 - 1, c);
					if (count1 != 0)
					{
						dest -= count1;
						cursor1 -= count1;
						len1 -= count1;
						CopyRange(a, cursor1 + 1, a, dest + 1, count1);
						if (len1 == 0)
							goto break_outer;
					}
					a[dest--] = tmp[cursor2--];
					if (--len2 == 1)
						goto break_outer;

					count2 = len2 - GallopLeft(a[cursor1], tmp, 0, len2, len2 - 1, c);
					if (count2 != 0)
					{
						dest -= count2;
						cursor2 -= count2;
						len2 -= count2;
						CopyRange(tmp, cursor2 + 1, a, dest + 1, count2);
						if (len2 <= 1)  // len2 == 1 || len2 == 0
							goto break_outer;
					}
					a[dest--] = a[cursor1--];
					if (--len1 == 0)
						goto break_outer;
					minGallop--;
				} while (count1 >= MIN_GALLOP | count2 >= MIN_GALLOP);

				if (minGallop < 0)
					minGallop = 0;
				minGallop += 2;  // Penalize for leaving gallop mode
			} // End of "outer" loop

			break_outer: // goto me! ;)

			m_MinGallop = minGallop < 1 ? 1 : minGallop;  // Write back to field

			if (len2 == 1)
			{
				Debug.Assert(len1 > 0);
				dest -= len1;
				cursor1 -= len1;
				CopyRange(a, cursor1 + 1, a, dest + 1, len1);
				a[dest] = tmp[cursor2];  // Move first elt of run2 to front of merge
			}
			else if (len2 == 0)
			{
				throw new ArgumentException("Comparison method violates its general contract!");
			}
			else
			{
				Debug.Assert(len1 == 0);
				Debug.Assert(len2 > 0);
				CopyRange(tmp, 0, a, dest - (len2 - 1), len2);
			}
		}

		/// <summary>
		/// Ensures that the external array tmp has at least the specified
		/// number of elements, increasing its size if necessary.  The size
		/// increases exponentially to ensure amortized linear time complexity.
		/// </summary>
		/// <param name="minCapacity">the minimum required capacity of the tmp array.</param>
		/// <returns>tmp, whether or not it grew</returns>
		private T[] EnsureCapacity(int minCapacity)
		{
			if (m_MergeBuffer.Length < minCapacity)
			{
				// Compute smallest power of 2 > minCapacity
				int newSize = minCapacity;
				newSize |= newSize >> 1;
				newSize |= newSize >> 2;
				newSize |= newSize >> 4;
				newSize |= newSize >> 8;
				newSize |= newSize >> 16;
				newSize++;

				if (newSize < 0) // Not bloody likely!
					newSize = minCapacity;
				else
					newSize = Math.Min(newSize, m_Array.Count >> 1);

				m_MergeBuffer = new T[newSize];
			}
			return m_MergeBuffer;
		}

		/// <summary>
		/// Checks that fromIndex and toIndex are in range, and throws an
		/// appropriate exception if they aren't.
		/// </summary>
		/// <param name="arrayLen">the length of the array.</param>
		/// <param name="fromIndex">the index of the first element of the range.</param>
		/// <param name="toIndex">the index after the last element of the range.</param>
		private static void CheckRange(int arrayLen, int fromIndex, int toIndex)
		{
			if (fromIndex > toIndex)
				throw new ArgumentException(string.Format("fromIndex({0}) > toIndex({1})", fromIndex, toIndex));
			if (fromIndex < 0)
				throw new IndexOutOfRangeException(string.Format("fromIndex ({0}) is out of bounds", fromIndex));
			if (toIndex > arrayLen)
				throw new IndexOutOfRangeException(string.Format("toIndex ({0}) is out of bounds", toIndex));
		}
	}

	#endregion
}
