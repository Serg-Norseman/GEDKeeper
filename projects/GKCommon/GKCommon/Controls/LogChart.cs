/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.Collections.Generic;
using System.Drawing;
using System.Windows.Forms;

namespace GKCommon.Controls
{
    public class HintRequestEventArgs : EventArgs
    {
        public int FragmentNumber { get; private set; }
        public int Size { get; private set; }
        public string Hint { get; set; }

        public HintRequestEventArgs(int fragmentNumber, int size)
        {
            this.FragmentNumber = fragmentNumber;
            this.Size = size;
        }
    }

    public delegate void HintRequestEventHandler(object sender, HintRequestEventArgs args);

    /// <summary>
    /// 
    /// </summary>
    public sealed class LogChart : Panel
    {
        private class Fragment
        {
            public int SrcVal;

            public double Val;
            public double Log;
            public double Percent;

            public int X;
            public int Width;
            
            public Rectangle Rect;
        }

        private static readonly Brush FRAG_BRUSH = new SolidBrush(Color.Green);
        private static readonly Brush EMPTY_BRUSH = new SolidBrush(Color.Gray);
        
        private readonly List<Fragment> fList;
        private readonly ToolTip fToolTip;
        private string fHint;

        public event HintRequestEventHandler OnHintRequest;

        public LogChart()
        {
            this.fList = new List<Fragment>();

            this.fToolTip = new ToolTip();
            this.fToolTip.AutoPopDelay = 5000;
            this.fToolTip.InitialDelay = 250;
            this.fToolTip.ReshowDelay = 50;
            this.fToolTip.ShowAlways = true;
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                FRAG_BRUSH.Dispose();
                EMPTY_BRUSH.Dispose();
            }
            base.Dispose(disposing);
        }

        public void Clear()
        {
            fList.Clear();
            this.UpdateContents();
        }

        public void AddFragment(int val)
        {
            if (val < 1) return;
            
            Fragment frag = new Fragment();

            frag.SrcVal = val;
            if (val == 1) val++;
            frag.Val = val;

            fList.Add(frag);

            this.UpdateContents();
        }

        private void UpdateContents()
        {
            int count = fList.Count;
            if (count == 0) return;
            
            int wid = this.Width - (count - 1);

            Fragment frag;

            // this is a calculation of the simple sum of fragments
            double sum = 0.0;
            for (int i = 0; i < count; i++) {
                frag = fList[i];
                sum = sum + frag.Val;
            }

            // the calculation of the logarithm of the fragment and the sum of the logarithms
            double logSum = 0.0;
            for (int i = 0; i < count; i++) {
                frag = fList[i];
                frag.Log = Math.Log(frag.Val, sum);

                logSum = logSum + frag.Log;
            }

            // calculate visual width of the fragments and their sum
            int resWidth = 0;
            for (int i = 0; i < count; i++) {
                frag = fList[i];
                frag.Percent = frag.Log / logSum;
                frag.Width = (int)(wid * frag.Percent);

                resWidth = resWidth + frag.Width;
            }

            // to distribute the difference between the actual width of the component and the sum of the width of the fragments
            int d = wid - resWidth;
            if (d > 0) {
                // the difference distribute between the highest allocated fragments
                List<Fragment> ordList = new List<Fragment>(this.fList);
                ordList.Sort(new FragmentComparer());

                int idx = 0;
                while (d > 0) {
                    frag = ordList[idx];
                    frag.Width = frag.Width + 1;

                    if (idx == count - 1) {
                        idx = 0;
                    } else idx++;
                    
                    d--;
                }
            }

            // prepare the regions of rendering fragments
            int x = 0;
            for (int i = 0; i < count; i++) {
                frag = fList[i];

                frag.X = x;
                frag.Rect = new Rectangle(x, 0, frag.Width, this.Height);
                x = x + (frag.Width + 1);
            }

            base.Invalidate();
        }

        private class FragmentComparer: IComparer<Fragment>
        {
            public int Compare(Fragment x, Fragment y)
            {
                return -x.Width.CompareTo(y.Width);
            }
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            base.OnPaint(e);
            
            if (this.Width <= 0 || this.Height <= 0) return;

            Graphics gfx = e.Graphics;

            int count = fList.Count;
            if (count > 0) {
                for (int i = 0; i < count; i++) {
                    Fragment frag = fList[i];
                    this.DrawRect(gfx, frag.X, frag.Width, FRAG_BRUSH);
                }
            } else {
                this.DrawRect(gfx, 0, this.Width, EMPTY_BRUSH);
            }
        }

        private void DrawRect(Graphics gfx, int x, int width, Brush lb)
        {
            gfx.FillRectangle(lb, x, 0, width, this.Height);
        }

        private string HintRequest(int fragmentNumber, int size)
        {
            if (this.OnHintRequest == null) return string.Empty;

            HintRequestEventArgs args = new HintRequestEventArgs(fragmentNumber, size);
            this.OnHintRequest(this, args);
            return args.Hint;
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            base.OnMouseMove(e);

            string hint = "";
            int count = fList.Count;
            for (int i = 0; i < count; i++) {
                Fragment frag = fList[i];

                if (frag.Rect.Contains(e.X, e.Y)) {
                    hint = HintRequest(i + 1, frag.SrcVal);
                    break;
                }
            }

            if (this.fHint != hint) {
                this.fHint = hint;
                fToolTip.Show(hint, this, e.X, e.Y, 3000);
            }
        }
    }
}
