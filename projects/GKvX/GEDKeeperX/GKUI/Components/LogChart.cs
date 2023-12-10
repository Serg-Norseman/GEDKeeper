/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2023 by Sergey V. Zhdanovskih.
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
using GKCore;
using SkiaSharp;
using SkiaSharp.Views.Forms;
using Xamarin.Forms;

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
    public class LogChart : ContentView
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

        private readonly SKCanvasView fCanvas;
        private readonly SKPaint fEmptyBrush;
        private readonly SKPaint fFragBrush;
        private readonly List<Fragment> fList;

        public event HintRequestEventHandler OnHintRequest;

        public LogChart()
        {
            fCanvas = new SKCanvasView();
            fCanvas.PaintSurface += OnPaint;
            Content = fCanvas;

            fList = new List<Fragment>();

            fEmptyBrush = new SKPaint {
                Color = SKColors.Gray,
                Style = SKPaintStyle.Fill
            };
            fFragBrush = new SKPaint {
                Color = SKColors.Green,
                Style = SKPaintStyle.Fill
            };
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

            int wid = (int)Width - (count - 1);
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

            fCanvas.InvalidateSurface();
        }

        private class FragmentComparer: IComparer<Fragment>
        {
            public int Compare(Fragment x, Fragment y)
            {
                return -x.Width.CompareTo(y.Width);
            }
        }

        private void OnPaint(object sender, SKPaintSurfaceEventArgs args)
        {
            var info = args.Info;
            var surface = args.Surface;
            var canvas = surface.Canvas;

            canvas.Clear();

            try {
                if (info.Width <= 0 || info.Height <= 0) return;

                int count = fList.Count;
                if (count > 0) {
                    for (int i = 0; i < count; i++) {
                        Fragment frag = fList[i];
                        DrawRect(canvas, frag.X, frag.Width, info.Height, fFragBrush);
                    }
                } else {
                    DrawRect(canvas, 0, info.Width, info.Height, fEmptyBrush);
                }
            } catch (Exception ex) {
                Logger.WriteError("LogChart.OnPaint()", ex);
            }
        }

        private void DrawRect(SKCanvas gfx, int x, int width, int height, SKPaint lb)
        {
            if (width > 0) {
                gfx.DrawRect(SKRect.Create(x, 0, width, height), lb);
            }
        }
    }
}
