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
        	this.LS_LifeGame = "Игра \"Жизнь Конвея\"";
        	this.LS_Step = "Шаг";
        	this.LS_Start = "Старт";
        	this.LS_Stop = "Стоп";
        	this.LS_SetCells = "Изменить поле";
        	this.LS_Clear = "Очистить";
        	this.LS_Random = "Случайное заполнение";
        	this.LS_Options = "Настройки";

        	this.RestoreDefaults();
        }
		
        public void RestoreDefaults()
        {
            this.AnimationDelay = LifeConsts.DefaultAnimationDelay;
            this.BackgroundColor = LifeConsts.DefaultBackgroundColor;
            this.GridHeight = LifeConsts.DefaultGridHeight;
            this.GridWidth = LifeConsts.DefaultGridWidth;
            this.LivingCellColor = LifeConsts.DefaultCellColor;
            this.fModified = true;
        }
    }
}