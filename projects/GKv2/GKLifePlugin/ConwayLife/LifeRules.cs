/*
 *  ULife, the old computer simulation of Life.
 *  Copyright (C) 1998 by Ian Lane (email: lanei@ideal.net.au)
 *
 *  Distribution: This control is free for public use and components may be
 *  freely descended from it as long as credit is given to the author.
 * 
 *  Converted to C#: 20/07/2011, Serg V. Zhdanovskih
 */

namespace GKLifePlugin.ConwayLife
{
    public sealed class LifeRules
    {
        private bool[] fDeadCells;
        private bool[] fLiveCells;
        private bool fModified;

        public bool Modified
        {
            get { return fModified; }
        }
        
        public bool GetDeadCells(int index)
        {
            return fDeadCells[index];
        }
        
        public bool GetLiveCells(int index)
        {
            return fLiveCells[index];
        }
        
        public void SetDeadCells(int index, bool value)
        {
            if (value != fDeadCells[index]) {
                fDeadCells[index] = value;
                fModified = true;
            }
        }
        
        public void SetLiveCells(int index, bool value)
        {
            if (value != fLiveCells[index]) {
                fLiveCells[index] = value;
                fModified = true;
            }
        }
        
        public LifeRules()
        {
            RestoreDefaults();
        }
        
        public void RestoreDefaults()
        {
            fDeadCells = (bool[])LifeConsts.DefaultDeadCells.Clone();
            fLiveCells = (bool[])LifeConsts.DefaultLiveCells.Clone();
            fModified = true;
        }
    }
}
