/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using GKCore.Design.Controls;
using Terminal.Gui;

namespace GKUI.Components
{
    using tgAttribute = Terminal.Gui.Attribute;

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
    public sealed class LogChart : View, ILogChart
    {
        private class Fragment
        {
            public int SrcVal;

            public double Val;
            public double Log;
            public double Percent;

            public int X;
            public int Width;

            public Rect Rect;

            public override string ToString()
            {
                return $"{X}, {Width}";
            }
        }

        private readonly List<Fragment> fList;
        //private readonly ToolTip fToolTip;
        private string fHint;

        public event HintRequestEventHandler OnHintRequest;

        public LogChart()
        {
            fList = new List<Fragment>();

            /*fToolTip = new ToolTip();
            fToolTip.AutoPopDelay = 5000;
            fToolTip.InitialDelay = 250;
            fToolTip.ReshowDelay = 50;
            fToolTip.ShowAlways = true;*/
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                //fToolTip.Dispose();
            }
            base.Dispose(disposing);
        }

        public new void Clear()
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

            int wid = Bounds.Width - (count - 1);
            if (wid <= 0) return;

            Fragment frag;

            // this is a calculation of the simple sum of fragments
            double sum = 0.0;
            for (int i = 0; i < count; i++) {
                frag = fList[i];
                sum += frag.Val;
            }

            // the calculation of the logarithm of the fragment and the sum of the logarithms
            double logSum = 0.0;
            for (int i = 0; i < count; i++) {
                frag = fList[i];
                frag.Log = Math.Log(frag.Val, sum);

                logSum += frag.Log;
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
                frag.Rect = new Rect(x, 0, frag.Width, 1);
                x = x + (frag.Width + 1);
            }

            SetNeedsDisplay();
        }

        private class FragmentComparer : IComparer<Fragment>
        {
            public int Compare(Fragment x, Fragment y)
            {
                return -x.Width.CompareTo(y.Width);
            }
        }

        public override void Redraw(Rect bounds)
        {
            if (Bounds.IsEmpty) return;

            base.Clear();

            int count = fList.Count;
            if (count > 0) {
                Driver.SetAttribute(tgAttribute.Make(Color.BrightCyan, Color.Blue));
                for (int i = 0; i < count; i++) {
                    Fragment frag = fList[i];
                    DrawRect(Driver, frag.X, frag.Width, false);
                }
            } else {
                Driver.SetAttribute(tgAttribute.Make(Color.Black, Color.Gray));
                DrawRect(Driver, 0, Bounds.Width, true);
            }
        }

        private void DrawRect(ConsoleDriver driver, int x, int width, bool empty)
        {
            if (width <= 0)
                return;

            Move(x, 0);
            if (empty) {
                for (int c = 0; c < width; c++) {
                    driver.AddRune(ConsoleDriver.Stipple);
                }
            } else {
                if (width == 1) {
                    driver.AddRune('\u25a0');
                } else {
                    for (int c = 0; c < width; c++) {
                        if (c == 0) {
                            driver.AddRune('\u2590');
                        } else if (c == width - 1) {
                            driver.AddRune('\u258c');
                        } else {
                            driver.AddRune(driver.FullBlock);
                        }
                    }
                }
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

        /*protected override void OnMouseMove(MouseEventArgs e)
        {
            base.OnMouseMove(e);
            Point mpt = e.Location;

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
                fToolTip.Show(hint, this, e.X, e.Y, 3000);
            }
        }*/

        public void Activate()
        {
            SetFocus();
        }
    }
}
