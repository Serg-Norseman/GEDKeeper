using System.Drawing;
using GKCommon;

namespace GKLifePlugin
{
    public sealed class LifeOptions
    {
        private int fAnimationDelay;
        private Color fBackgroundColor;
        private Color fDisplayedInformationColor;
        private bool fDisplayGeneration;
        private bool fDisplayLivingCells;
        private int fGridHeight;
        private int fGridWidth;
        private Color fLivingCellColor;
        private bool fModified;
		
		
        public int AnimationDelay
        {
            get { return this.fAnimationDelay; }
            set {
                if (value != fAnimationDelay) {
                    this.fAnimationDelay = value;
                    this.fModified = true;
                }
            }
        }

        public Color BackgroundColor
        {
            get { return this.fBackgroundColor; }
            set {
                if (value != this.fBackgroundColor) {
                    this.fBackgroundColor = value;
                    this.fModified = true;
                }
            }
        }
		
        public Color DisplayedInformationColor
        {
            get { return this.fDisplayedInformationColor; }
            set {
                if (value != this.fDisplayedInformationColor) {
                    this.fDisplayedInformationColor = value;
                    this.fModified = true;
                }
            }
        }
		
        public bool DisplayGeneration
        {
            get { return this.fDisplayGeneration; }
            set {
                if (value != fDisplayGeneration) {
                    fDisplayGeneration = value;
                    fModified = true;
                }
            }
        }
		
        public bool DisplayLivingCells
        {
            get { return this.fDisplayLivingCells; }
            set {
                if (value != this.fDisplayLivingCells) {
                    this.fDisplayLivingCells = value;
                    this.fModified = true;
                }
            }
        }
		
        public int GridHeight
        {
            get { return this.fGridHeight; }
            set {
                if (value != this.fGridHeight) {
                    this.fGridHeight = value;
                    this.fModified = true;
                }
            }
        }
		
        public int GridWidth
        {
            get { return this.fGridWidth; }
            set {
                if (value != this.fGridWidth) {
                    this.fGridWidth = value;
                    this.fModified = true;
                }
            }
        }
		
        public Color LivingCellColor
        {
            get { return this.fLivingCellColor; }
            set {
                if (value != this.fLivingCellColor) {
                    this.fLivingCellColor = value;
                    this.fModified = true;
                }
            }
        }
		
        public bool Modified
        {
            get { return this.fModified; }
        }

		
        public LifeOptions()
        {
            this.RestoreDefaults();
        }
		
        public void RestoreDefaults()
        {
            this.AnimationDelay = LifeConsts.DefaultAnimationDelay;
            this.BackgroundColor = LifeConsts.DefaultBackgroundColor;
            this.DisplayedInformationColor = LifeConsts.DefaultDisplayedInformationColor;
            this.DisplayGeneration = LifeConsts.DefaultDisplayGeneration;
            this.DisplayLivingCells = LifeConsts.DefaultDisplayLivingCells;
            this.GridHeight = LifeConsts.DefaultGridHeight;
            this.GridWidth = LifeConsts.DefaultGridWidth;
            this.LivingCellColor = LifeConsts.DefaultLivingCellColor;
        }
		
        public void Load(IniFile iniFile)
        {
			
        }
		
        public void Save(IniFile iniFile)
        {
			
        }
    }
}