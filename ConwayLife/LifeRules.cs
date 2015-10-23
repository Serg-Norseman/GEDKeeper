/*
 *  ULife
 *  Author: Ian Lane (email: lanei@ideal.net.au)
 *  Copyright (C) 1998 Ian Lane
 *
 *  Synopsis: A Delphi control which implements the old computer simulation
 *  of Life. Useful for about boxes, screen savers or even as the
 *  core of a "Life" application.
 *
 *  Distribution: This control is free for public use and components may be
 *  freely descended from it as long as credit is given to the author.
 * 
 *  Converted to C#: 20/07/2011, Serg V. Zhdanovskih
 */

namespace ConwayLife
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