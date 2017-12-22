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
    /// Object that can be placed in a treemap layout.
    /// </summary>
    public class MapItem
    {
        private MapRect fBounds;
        private double fCalcSize;
        private int fDepth;
        private readonly List<MapItem> fItems;
        private string fName;
        private int fOrder = 0;
        private double fRatio;
        private double fSize;

        public MapRect Bounds
        {
            get { return fBounds; }
            set { fBounds = value; }
        }

        public int Depth
        {
            get { return fDepth; }
            set { fDepth = value; }
        }

        public string Name
        {
            get { return fName; }
            set { fName = value; }
        }

        public int Order
        {
            get { return fOrder; }
            set { fOrder = value; }
        }

        public double Ratio
        {
            get { return fRatio; }
            set { fRatio = value; }
        }

        public double Size
        {
            get { return fSize; }
            set { fSize = value; }
        }

        public List<MapItem> Items
        {
            get { return fItems; }
        }

        public MapItem()
            : this(1, 0)
        {
        }

        public MapItem(double size, int order)
        {
            fSize = size;
            fOrder = order;
            fBounds = new MapRect();
        }

        public MapItem(string name, double size)
        {
            fItems = new List<MapItem>();
            fName = name;
            fSize = size;
            fBounds = new MapRect();
            fCalcSize = 0;
        }

        public double GetCalcSize()
        {
            return fCalcSize;
        }

        public void CalculateSize()
        {
            if (IsLeaf()) {
                fCalcSize = fSize;
            } else {
                double tempSum = 0;
                foreach (MapItem item in fItems) {
                    item.CalculateSize();
                    tempSum += item.fCalcSize;
                }
                fCalcSize = tempSum;
            }
        }

        public void SetBounds(double x, double y, double w, double h)
        {
            fBounds.SetRect(x, y, w, h);
        }

        public void AddItem(MapItem item)
        {
            fItems.Add(item);
        }

        public bool Contains(int x, int y)
        {
            return fBounds.Contains(x, y);
        }

        public MapItem FindByCoord(int x, int y)
        {
            bool res = Contains(x, y);
            if (!res) {
                return null;
            }

            if (fItems.Count <= 0) {
                return this;
            }

            foreach (MapItem item in fItems) {
                MapItem found = item.FindByCoord(x, y);
                if (found != null) {
                    return found;
                }
            }

            return null;
        }

        public bool IsLeaf()
        {
            return fItems.Count <= 0;
        }
    }
}
