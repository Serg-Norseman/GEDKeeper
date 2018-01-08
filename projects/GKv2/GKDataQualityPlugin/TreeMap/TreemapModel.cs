/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017 by Sergey V. Zhdanovskih.
 *
 *  This file is part of "GEDKeeper".
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

using System.Collections.Generic;

namespace GKCommon.TreeMap
{
    /// <summary>
    /// TreemapModel. Model object used to represent data for a treemap.
    /// 
    /// Implements the Squarified Treemap layout published by Mark Bruls, Kees
    /// Huizing, and Jarke J. van Wijk
    ///
    /// Squarified Treemaps https://www.win.tue.nl/~vanwijk/stm.pdf
    /// </summary>
    public abstract class TreemapModel
    {
        private readonly List<MapItem> fItems;
        private readonly int fHeight;
        private readonly int fWidth;

        /// <summary>
        /// Creates a Map Model instance based on the relative size of the mappable items and the frame size.
        /// </summary>
        /// <param name="width">Width of the display area</param>
        /// <param name="height">Height of the display area</param>
        protected TreemapModel(int width, int height)
        {
            fItems = new List<MapItem>();
            fWidth = width;
            fHeight = height;
        }

        public void Clear()
        {
            fItems.Clear();
        }

        public abstract MapItem newItem(string name, double size);

        public MapItem CreateItem(MapItem parent, string name, double size)
        {
            MapItem result = newItem(name, size);
            if (parent == null) {
                fItems.Add(result);
            } else {
                parent.AddItem(result);
            }
            return result;
        }

        /// <summary>
        /// Get the list of items in this model.
        /// </summary>
        /// <returns>An array of the MapItem objects in this MapModel.</returns>
        public List<MapItem> GetItems()
        {
            return fItems;
        }

        public MapItem FindByCoord(int x, int y)
        {
            foreach (MapItem item in fItems) {
                MapItem found = item.FindByCoord(x, y);
                if (found != null) {
                    return found;
                }
            }

            return null;
        }

        /// <summary>
        /// Arrange the items in the given MapModel to fill the given rectangle.
        /// </summary>
        /// <param name="bounds">The bounding rectangle for the layout.</param>
        public void CalcLayout(MapRect bounds)
        {
            // calculate all true sizes for treemap
            foreach (MapItem item in fItems) {
                item.CalculateSize();
            }

            // calculate bounds of item for all levels
            CalcRecursiveLayout(fItems, bounds);
        }

        public void CalcRecursiveLayout(List<MapItem> itemsList, MapRect bounds)
        {
            CalcLayout(itemsList, bounds);

            foreach (MapItem item in itemsList) {
                if (item.IsLeaf()) {
                    continue;
                }

                CalcRecursiveLayout(item.Items, item.Bounds);
            }
        }

        public void CalcLayout(List<MapItem> itemsList, MapRect bounds)
        {
            if (itemsList == null || itemsList.Count <= 0) {
                return;
            }

            // calculate sum for current level
            double sum = 0;
            foreach (MapItem item in itemsList) {
                sum += item.GetCalcSize();
            }

            // calculate relative sizes for current level
            double totalArea = bounds.W * bounds.H;
            foreach (MapItem item in itemsList) {
                item.Ratio = (totalArea / sum * item.GetCalcSize());
            }

            MapItem[] itemsArr = new MapItem[itemsList.Count];
            itemsArr = itemsList.ToArray();
            QuickSortDesc(itemsArr, 0, itemsArr.Length - 1);

            CalcLayout(itemsArr, 0, itemsArr.Length - 1, bounds);
        }

        private void CalcLayout(MapItem[] items, int start, int end, MapRect bounds)
        {
            if (start > end) {
                return;
            }
            if (start == end) {
                items[start].Bounds = bounds;
            }

            int mid = start;
            while (mid < end) {
                if (GetHighestAspect(items, start, mid, bounds) > GetHighestAspect(items, start, mid + 1, bounds)) {
                    mid++;
                } else {
                    MapRect newBounds = LayoutRow(items, start, mid, bounds);
                    CalcLayout(items, mid + 1, end, newBounds);
                }
            }
        }

        private double GetHighestAspect(MapItem[] items, int start, int end, MapRect bounds)
        {
            LayoutRow(items, start, end, bounds);
            double max = double.MinValue;
            for (int i = start; i <= end; i++) {
                if (items[i].Bounds.GetAspectRatio() > max) {
                    max = items[i].Bounds.GetAspectRatio();
                }
            }
            return max;
        }

        private MapRect LayoutRow(MapItem[] items, int start, int end, MapRect bounds)
        {
            bool isHorizontal = bounds.W > bounds.H;
            double total = bounds.W * bounds.H; //totalSize(items, 0, items.length-1);
            double rowSize = GetTotalSize(items, start, end);
            double rowRatio = rowSize / total;
            double offset = 0;

            for (int i = start; i <= end; i++) {
                MapRect r = new MapRect();
                double ratio = items[i].Ratio / rowSize;

                if (isHorizontal) {
                    r.X = bounds.X;
                    r.W = bounds.W * rowRatio;
                    r.Y = bounds.Y + bounds.H * offset;
                    r.H = bounds.H * ratio;
                } else {
                    r.X = bounds.X + bounds.W * offset;
                    r.W = bounds.W * ratio;
                    r.Y = bounds.Y;
                    r.H = bounds.H * rowRatio;
                }
                items[i].Bounds = r;
                offset += ratio;
            }
            if (isHorizontal) {
                return new MapRect(bounds.X + bounds.W * rowRatio, bounds.Y, bounds.W - bounds.W * rowRatio, bounds.H);
            } else {
                return new MapRect(bounds.X, bounds.Y + bounds.H * rowRatio, bounds.W, bounds.H - bounds.H * rowRatio);
            }
        }

        private static double GetTotalSize(MapItem[] items, int start, int end)
        {
            double sum = 0;
            for (int i = start; i <= end; i++) {
                sum += items[i].Ratio;
            }
            return sum;
        }

        private void QuickSortDesc(MapItem[] inputArr, int lowerIndex, int higherIndex)
        {
            int i = lowerIndex;
            int j = higherIndex;
            // calculate pivot number
            double pivot = inputArr[lowerIndex + (higherIndex - lowerIndex) / 2].Ratio;
            // Divide into two arrays
            while (i <= j) {
                /**
                 * In each iteration, we will identify a number from left side which
                 * is greater then the pivot value, and also we will identify a
                 * number from right side which is less then the pivot value. Once
                 * the search is done, then we exchange both numbers.
                 */
                while (inputArr[i].Ratio > pivot) {
                    i++;
                }
                while (inputArr[j].Ratio < pivot) {
                    j--;
                }
                if (i <= j) {
                    MapItem temp = inputArr[i];
                    inputArr[i] = inputArr[j];
                    inputArr[j] = temp;
                    //move index to next position on both sides
                    i++;
                    j--;
                }
            }
            // call quickSort() method recursively
            if (lowerIndex < j) {
                QuickSortDesc(inputArr, lowerIndex, j);
            }
            if (i < higherIndex) {
                QuickSortDesc(inputArr, i, higherIndex);
            }
        }
    }
}
