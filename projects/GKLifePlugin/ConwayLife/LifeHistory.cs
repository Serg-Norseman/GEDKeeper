/*
 *  ULife, the old computer simulation of Life.
 *  Copyright (C) 1998 by Ian Lane (email: lanei@ideal.net.au)
 *
 *  Distribution: This control is free for public use and components may be
 *  freely descended from it as long as credit is given to the author.
 * 
 *  Converted to C#: 20/07/2011, Serg V. Zhdanovskih
 */

using System;
using System.Collections.Generic;

namespace ConwayLife
{
    public sealed class LifeHistory : IDisposable
    {
        private List<LifeGrid> fList;
        private int fMostRecent;

        public int Count
        {
            get { return fList.Count; }
        }

        public LifeGrid this[int Index]
        {
            get {
                int size = fList.Count;
                return fList[(fMostRecent + size - Index) % size];
            }
        }

        public int MaxLevels
        {
            get { return fList.Capacity; }
            set {
                Clear();
                fList.Capacity = value;
            }
        }
        
        public LifeHistory(int maxLevels)
        {
            fList = new List<LifeGrid>();
            fList.Capacity = maxLevels;
        }

        public void Dispose()
        {
            Clear();
            fList = null;
        }
        
        public LifeGrid Add(LifeGrid grid)
        {
            if (fList.Count < fList.Capacity) {
                fList.Add(new LifeGrid());
                fMostRecent = fList.Count - 1;
            } else {
                fMostRecent = (fMostRecent + 1) % Count;
            }

            LifeGrid result = fList[fMostRecent];
            result.Assign(grid);
            return result;
        }
        
        public void Clear()
        {
            for (int i = 0; i < fList.Count; i++) fList[i].Dispose();
            fList.Clear();
        }

        public int Contains(LifeGrid grid)
        {
            int result = 0;
            while ((result < fList.Count) && !grid.Equals(this[result])) result++;
            if (result >= fList.Count) result = -1;
            
            return result;
        }
    }
}