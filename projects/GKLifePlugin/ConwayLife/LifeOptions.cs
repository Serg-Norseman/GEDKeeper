/*
 *  ULife, the old computer simulation of Life.
 *  Copyright (C) 1998 by Ian Lane (email: lanei@ideal.net.au)
 *
 *  Distribution: This control is free for public use and components may be
 *  freely descended from it as long as credit is given to the author.
 * 
 *  Converted to C#: 20/07/2011, Serg V. Zhdanovskih
 */

using System.Drawing;

namespace ConwayLife
{
    public sealed class LifeOptions
    {
        private int fAnimationDelay;
        private Color fBackgroundColor;
        private int fGridHeight;
        private int fGridWidth;
        private Color fLivingCellColor;
        private bool fModified;
        
        public string LS_LifeGame { get; set; }
        public string LS_Step { get; set; }
        public string LS_Start { get; set; }
        public string LS_Stop { get; set; }
        public string LS_SetCells { get; set; }
        public string LS_Clear { get; set; }
        public string LS_Random { get; set; }
        public string LS_Options { get; set; }
        
        public int AnimationDelay
        {
            get { return fAnimationDelay; }
            set {
                if (value != fAnimationDelay) {
                    fAnimationDelay = value;
                    fModified = true;
                }
            }
        }

        public Color BackgroundColor
        {
            get { return fBackgroundColor; }
            set {
                if (value != fBackgroundColor) {
                    fBackgroundColor = value;
                    fModified = true;
                }
            }
        }
        
        public int GridHeight
        {
            get { return fGridHeight; }
            set {
                if (value != fGridHeight) {
                    fGridHeight = value;
                    fModified = true;
                }
            }
        }
        
        public int GridWidth
        {
            get { return fGridWidth; }
            set {
                if (value != fGridWidth) {
                    fGridWidth = value;
                    fModified = true;
                }
            }
        }
        
        public Color LivingCellColor
        {
            get { return fLivingCellColor; }
            set {
                if (value != fLivingCellColor) {
                    fLivingCellColor = value;
                    fModified = true;
                }
            }
        }
        
        public bool Modified
        {
            get { return fModified; }
        }

        
        public LifeOptions()
        {
            LS_LifeGame = "Игра \"Жизнь Конвея\"";
            LS_Step = "Шаг";
            LS_Start = "Старт";
            LS_Stop = "Стоп";
            LS_SetCells = "Изменить поле";
            LS_Clear = "Очистить";
            LS_Random = "Случайное заполнение";
            LS_Options = "Настройки";

            RestoreDefaults();
        }
        
        public void RestoreDefaults()
        {
            AnimationDelay = LifeConsts.DefaultAnimationDelay;
            BackgroundColor = LifeConsts.DefaultBackgroundColor;
            GridHeight = LifeConsts.DefaultGridHeight;
            GridWidth = LifeConsts.DefaultGridWidth;
            LivingCellColor = LifeConsts.DefaultCellColor;
            fModified = true;
        }
    }
}