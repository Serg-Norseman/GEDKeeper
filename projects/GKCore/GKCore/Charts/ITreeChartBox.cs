/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2018 by Sergey V. Zhdanovskih.
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
using BSLib;
using GKCommon.GEDCOM;
using GKCore.Interfaces;
using GKCore.Options;

namespace GKCore.Charts
{
    public enum ChartControlMode
    {
        ccmDefault,
        ccmDragImage,
        ccmControlsVisible
    }

    public enum MouseAction
    {
        maNone,
        maSelect,
        maExpand,
        maDrag,
        maProperties,
        maHighlight,
        maPersonExpand
    }

    public enum MouseEvent
    {
        meDown,
        meMove,
        meUp
    }

    public interface ITreeChartBox
    {
        IBaseWindow Base { get; set; }
        int DepthLimit { get; set; }
        int Height { get; set; }
        TreeChartKind Kind { get; set; }
        TreeChartModel Model { get; }
        TreeChartOptions Options { get; set; }
        float Scale { get; }
        TreeChartPerson Selected { get; set; }
        int Width { get; set; }

        void GenChart(GEDCOMIndividualRecord iRec, TreeChartKind kind, bool rootCenter);
        ExtRect GetClientRect();
        ExtSize GetImageSize();
        ExtPoint GetOffsets();
        void Invalidate();
        void RefreshTree();
        void RenderImage(RenderTarget target, bool forciblyCentered = false);
        void SaveSnapshot(string fileName);
        void SetRenderer(ChartRenderer renderer);
        void SetScale(float value);
    }


    /// <summary>
    /// 
    /// </summary>
    public abstract class ITreeControl : BaseObject
    {
        protected readonly ITreeChartBox fChart;

        protected ExtRect fDestRect;
        protected bool fMouseCaptured;
        protected bool fVisible;

        public bool MouseCaptured
        {
            get { return fMouseCaptured; }
        }

        public bool Visible
        {
            get {
                return fVisible;
            }
            set {
                if (fVisible != value) {
                    fVisible = value;
                    fChart.Invalidate();
                }
            }
        }

        public abstract string Tip { get; }
        public abstract int Height { get; }
        public abstract int Width { get; }

        public abstract void UpdateState();
        public abstract void UpdateView();
        public abstract void Draw(ChartRenderer gfx);
        public abstract void MouseDown(int x, int y);
        public abstract void MouseMove(int x, int y);
        public abstract void MouseUp(int x, int y);

        protected ITreeControl(ITreeChartBox chart)
        {
            fChart = chart;
        }

        public virtual bool Contains(int x, int y)
        {
            return fDestRect.Contains(x, y);
        }
    }


    public sealed class TreeControlsList<T> : List<T>, IDisposable where T : ITreeControl
    {
        public void Draw(ChartRenderer gfx)
        {
            if (gfx == null) return;

            for (int i = 0; i < Count; i++) {
                T ctl = this[i];
                if (ctl.Visible) ctl.Draw(gfx);
            }
        }

        public void UpdateState()
        {
            for (int i = 0; i < Count; i++) {
                this[i].UpdateState();
            }
        }

        public void UpdateView()
        {
            for (int i = 0; i < Count; i++) {
                this[i].UpdateView();
            }
        }

        public ITreeControl Contains(int x, int y)
        {
            for (int i = 0; i < Count; i++) {
                ITreeControl ctl = this[i];
                if (ctl.Contains(x, y)) return ctl;
            }

            return null;
        }

        public void MouseDown(int x, int y)
        {
            for (int i = 0; i < Count; i++) {
                T ctl = this[i];
                if (ctl.Visible) ctl.MouseDown(x, y);
            }
        }

        public void MouseMove(int x, int y, bool defaultChartMode)
        {
            for (int i = 0; i < Count; i++) {
                T ctl = this[i];
                if (ctl.Visible) ctl.MouseMove(x, y);
            }
        }

        public void MouseUp(int x, int y)
        {
            for (int i = 0; i < Count; i++) {
                T ctl = this[i];
                if (ctl.Visible) ctl.MouseUp(x, y);
            }
        }

        public void Dispose()
        {
            for (int i = 0; i < Count; i++) {
                this[i].Dispose();
            }
            Clear();
        }
    }
}
