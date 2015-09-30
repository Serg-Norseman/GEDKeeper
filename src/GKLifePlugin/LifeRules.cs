namespace GKLifePlugin
{
    public sealed class LifeRules
    {
        private bool[] fDeadCells;
        private bool[] fLiveCells;
        private bool fModified;

        public bool Modified
        {
            get { return this.fModified; }
        }
		
        public bool GetDeadCells(int index)
        {
            return this.fDeadCells[index];
        }
		
        public bool GetLiveCells(int index)
        {
            return this.fLiveCells[index];
        }
		
        public void SetDeadCells(int index, bool value)
        {
            if (value != this.fDeadCells[index]) {
                this.fDeadCells[index] = value;
                this.fModified = true;
            }
        }
		
        public void SetLiveCells(int index, bool value)
        {
            if (value != this.fLiveCells[index]) {
                this.fLiveCells[index] = value;
                this.fModified = true;
            }
        }
		
        public LifeRules()
        {
            RestoreDefaults();
        }
		
        public void RestoreDefaults()
        {
            this.fDeadCells = (bool[])LifeConsts.DefaultDeadCells.Clone();
            this.fLiveCells = (bool[])LifeConsts.DefaultLiveCells.Clone();
            this.fModified = true;
        }
		
        public void Load()
        {
			
        }
		
        public void Save()
        {
			
        }
    }
}