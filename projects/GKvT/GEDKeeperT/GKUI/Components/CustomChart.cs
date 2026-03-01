/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using BSLib;
using GKCore.Charts;
using GKCore.Design.Graphics;
using Terminal.Gui;

namespace GKUI.Components
{
    public abstract class CustomChart : ScrollablePanel, IPrintable
    {
        private readonly NavigationStack<object> fNavman;
        protected IChartLayout fLayout;
        protected ChartRenderer fRenderer;


        public event EventHandler NavRefresh;

        public virtual float Scale
        {
            get { return 1.0f; }
        }


        protected CustomChart()
        {
            CenteredImage = true;

            fNavman = new NavigationStack<object>();
        }

        public virtual void SetScale(float value)
        {
        }

        public override bool OnKeyDown(KeyEvent keyEvent)
        {
            if (keyEvent.Key == Key.Backspace) {
                NavPrev();
                return true;
            } else return base.OnKeyDown(keyEvent);
        }

        #region Print and snaphots support

        public abstract ExtSize GetImageSize();
        public abstract void RenderImage(RenderTarget target, bool forciblyCentered = false);

        public bool IsLandscape()
        {
            ExtSize imageSize = GetImageSize();
            return (imageSize.Height < imageSize.Width);
        }

        public IImage GetPrintableImage()
        {
            return null;
        }

        public void CopySnapshot()
        {
        }

        public void SaveSnapshot(string fileName)
        {
        }

        public virtual void SetLayout(IChartLayout layout)
        {
            fLayout = layout;
        }

        public virtual void SetRenderer(ChartRenderer renderer)
        {
            fRenderer = renderer;
        }

        #endregion

        #region Navigation support

        private void DoNavRefresh()
        {
            NavRefresh?.Invoke(this, null);
        }

        protected abstract void SetNavObject(object obj);

        public bool NavAdd(object obj)
        {
            if (obj != null) {
                fNavman.Current = obj;
                return true;
            }
            return false;
        }

        public bool NavCanBackward()
        {
            return fNavman.CanBackward();
        }

        public bool NavCanForward()
        {
            return fNavman.CanForward();
        }

        public void NavNext()
        {
            if (!fNavman.CanForward()) return;

            SetNavObject(fNavman.Next());
            DoNavRefresh();
        }

        public void NavPrev()
        {
            if (!fNavman.CanBackward()) return;

            SetNavObject(fNavman.Back());
            DoNavRefresh();
        }

        #endregion
    }
}
