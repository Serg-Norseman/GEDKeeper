/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Components
{
    public class HintRequestEventArgs : EventArgs
    {
        public int FragmentNumber { get; private set; }
        public int Size { get; private set; }
        public string Hint { get; set; }

        public HintRequestEventArgs(int fragmentNumber, int size)
        {
            FragmentNumber = fragmentNumber;
            Size = size;
        }
    }

    public delegate void HintRequestEventHandler(object sender, HintRequestEventArgs args);

    /// <summary>
    /// Logarithmic graph of fragmented data.
    /// </summary>
    public sealed class LogChart : Drawable
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

        private readonly Brush fEmptyBrush;
        private readonly Brush fFragBrush;
        private readonly List<Fragment> fList;
        private string fHint;

        public event HintRequestEventHandler OnHintRequest;

        public LogChart()
        {
            fList = new List<Fragment>();

            fEmptyBrush = new SolidBrush(Colors.Gray);
            fFragBrush = new SolidBrush(Colors.Green);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fFragBrush.Dispose();
                fEmptyBrush.Dispose();
            }
            base.Dispose(disposing);
        }

        public void Clear()
        {
            fList.Clear();
            UpdateContents();
        }

        public void AddFragment(int val)
        {
            if (val < 1) return;
            
            Fragment frag = new Fragment();

            frag.SrcVal = val;
            if (val == 1) val++;
            frag.Val = val;

            fList.Add(frag);

            UpdateContents();
        }

        private void UpdateContents()
        {
            int count = fList.Count;
            if (count == 0) return;

            int wid = Width - (count - 1);
            if (wid <= 0) return;

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
                List<Fragment> ordList = new List<Fragment>(fList);
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
                frag.Rect = new Rectangle(x, 0, frag.Width, Height);
                x = x + (frag.Width + 1);
            }

            Invalidate();
        }

        private class FragmentComparer: IComparer<Fragment>
        {
            public int Compare(Fragment x, Fragment y)
            {
                return -x.Width.CompareTo(y.Width);
            }
        }

        protected override void OnSizeChanged(EventArgs e)
        {
            base.OnSizeChanged(e);
            UpdateContents();
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            base.OnPaint(e);

            if (Width <= 0 || Height <= 0) return;

            Graphics gfx = e.Graphics;

            int count = fList.Count;
            if (count > 0) {
                for (int i = 0; i < count; i++) {
                    Fragment frag = fList[i];
                    DrawRect(gfx, frag.X, frag.Width, fFragBrush);
                }
            } else {
                DrawRect(gfx, 0, Width, fEmptyBrush);
            }
        }

        private void DrawRect(Graphics gfx, int x, int width, Brush lb)
        {
            if (width > 0) {
                gfx.FillRectangle(lb, x, 0, width, Height);
            }
        }

        private string HintRequest(int fragmentNumber, int size)
        {
            var onHintRequest = OnHintRequest;
            if (onHintRequest == null) return string.Empty;

            HintRequestEventArgs args = new HintRequestEventArgs(fragmentNumber, size);
            onHintRequest(this, args);
            return args.Hint;
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            Point mpt = new Point(e.Location);

            string hint = "";
            int count = fList.Count;
            for (int i = 0; i < count; i++) {
                Fragment frag = fList[i];

                if (frag.Rect.Contains(mpt.X, mpt.Y)) {
                    hint = HintRequest(i + 1, frag.SrcVal);
                    break;
                }
            }

            if (fHint != hint) {
                fHint = hint;
                //fToolTip.Show(hint, this, mpt.X, mpt.Y, 3000);
                ToolTip = hint;
            }

            e.Handled = true;
            base.OnMouseMove(e);
        }
    }
}
