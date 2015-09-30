using System.Collections.Generic;

namespace GKLifePlugin
{
    public sealed class LifeHistory
    {
        private List<LifeGrid> fList;
        private int fMostRecent;

        public int Count
        {
            get { return this.fList.Count; }
        }

        public LifeGrid this[int Index]
        {
            get {
        		int size = this.fList.Count;
        		return this.fList[(this.fMostRecent + size - Index) % size];
        	}
        }

        public int MaxLevels
        {
            get { return this.fList.Capacity; }
            set {
                this.Clear();
                this.fList.Capacity = value;
            }
        }
		
        public LifeHistory(int maxLevels)
        {
            this.fList = new List<LifeGrid>();
            this.fList.Capacity = maxLevels;
        }

        public void Destroy()
        {
            this.Clear();
            this.fList = null;
        }
		
        public LifeGrid Add(LifeGrid grid)
        {
            if (this.fList.Count < this.fList.Capacity) {
                this.fList.Add(new LifeGrid());
                this.fMostRecent = this.fList.Count - 1;
            } else {
                this.fMostRecent = (this.fMostRecent + 1) % Count;
            }

            LifeGrid result = this.fList[this.fMostRecent];
            result.Assign(grid);
            return result;
        }
		
        public void Clear()
        {
            for (int i = 0; i < this.fList.Count; i++) this.fList[i].Destroy();
            this.fList.Clear();
        }

        public int Contains(LifeGrid grid)
        {
            int result = 0;
            while ((result < this.fList.Count) && !grid.Equals(this[result])) result++;
            if (result >= this.fList.Count) result = -1;
			
            return result;
        }
    }
}