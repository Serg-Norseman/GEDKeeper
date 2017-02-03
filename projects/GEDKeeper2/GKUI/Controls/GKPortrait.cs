/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2016 by Serg V. Zhdanovskih, Igor Tyulyakov.
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
 
using System;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;
using System.Collections.Generic;

namespace GKUI.Controls
{
    /// <summary>
    /// Image with the pop-up panel.
    /// </summary>
    public partial class GKPortrait : UserControl
    {
        public Image Image 
        { 
            get { return this.pictureBox1.Image; } 
            set { this.pictureBox1.Image = value; } 
        }
        
        public override Image BackgroundImage 
        {
            get { return this.pictureBox1.BackgroundImage; } 
            set { this.pictureBox1.BackgroundImage = value; } 
        }
        
        public int SlidePanelHeight
        { 
            get { return btnPanel.Height; } 
            set { btnPanel.Height = value; } 
        }
        
        public Panel SlidePanel 
        {
            get { return btnPanel; } 
            set { btnPanel = value; } 
        }
        
        public int PixelSpeed 
        { 
            get { return fPixelSpeed; } 
            set { fPixelSpeed = value; } 
        }
        
        public PictureBoxSizeMode SizeMode 
        { 
            get { return pictureBox1.SizeMode; } 
            set { pictureBox1.SizeMode = value;} 
        }
        
        public override Cursor Cursor
        {
            get {
                return base.Cursor;
            }
            set {
                base.Cursor = value;
                pictureBox1.Cursor = value;
                btnPanel.Cursor = value;
            }
        }
        
        private int fPixelSpeed = 5;
        private List<Button> fBtnsList = new List<Button>();        
        
        public GKPortrait()
        {
            InitializeComponent();
            btnPanel.Top = this.Height;
            timer.Interval = 1;
            timer.Stop();
        }
        
        public void AddButton(Button b)
        {
            fBtnsList.Add(b);
            ReDrawButtons();
        }

        private void ReDrawButtons()
        {
            int lenwagon = 0;

            btnPanel.Controls.Clear();

            for (int i = 0, c = fBtnsList.Count; i < c; i++) {
                lenwagon += (i != c) ? (fBtnsList[i].Width + 8) : fBtnsList[i].Width;
            }

            int center = lenwagon / 2;
            int startPosition = btnPanel.Width / 2 - center;
            
            for (int i = 0, c = fBtnsList.Count; i < c; i++) {
                int heightCenter = btnPanel.Height / 2;
                int btnCenter = fBtnsList[i].Height / 2;
                
                fBtnsList[i].Location = new System.Drawing.Point(startPosition, heightCenter - btnCenter);
                btnPanel.Controls.Add(fBtnsList[i]);
                startPosition += fBtnsList[i].Width + 8;
            }

        }

        private void MoveSlidePanel(object sender, EventArgs e)
        {
            if (btnPanel.Top <= this.Height - btnPanel.Height)
                timer.Stop();
            else 
                btnPanel.Top -= (btnPanel.Top - fPixelSpeed > this.Height - btnPanel.Height) ? fPixelSpeed : btnPanel.Top - (this.Height - btnPanel.Height);
        }
        
        private void MouseHoverOrLeave(object sender, EventArgs e)
        {
            Point p = this.PointToClient(Cursor.Position);

            if (p.X <= 0 || p.Y <= 0 || p.X >= pictureBox1.Width || p.Y >= pictureBox1.Height) {
                btnPanel.Top = this.Height;
                timer.Stop();
            }
            else {
                timer.Start();
            }
        }
        
        protected override void OnResize(EventArgs e)
        {
            base.OnResize(e);
            btnPanel.Width = this.Width;
            this.MouseHoverOrLeave(this, e);
        }
    }
}
