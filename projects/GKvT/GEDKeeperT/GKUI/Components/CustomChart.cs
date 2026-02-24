/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using BSLib;
using GKCore;
using GKCore.Charts;
using GKCore.Design.Graphics;
using GKCore.Export;
using GKCore.Export.Formats;
using GKCore.Locales;
using GKUI.Platform.Handlers;
using Terminal.Gui;

namespace GKUI.Components
{
    public abstract class CustomChart : ScrollablePanel, IPrintable
    {
        private readonly NavigationStack<object> fNavman;
        protected IChartLayout fLayout;
        protected ChartRenderer fRenderer;


        public event EventHandler NavRefresh;

        public new virtual float Scale
        {
            get { return 0; }
        }


        protected CustomChart()
        {
            CenteredImage = true;

            fNavman = new NavigationStack<object>();
        }

        public virtual void SetScale(float value)
        {
        }

        /*protected override void OnKeyDown(KeyEventArgs e)
        {
            e.Handled = true;
            switch (e.KeyCode) {
                case Keys.Left:
                    HorizontalScroll.Value =
                        Math.Max(HorizontalScroll.Value - HorizontalScroll.SmallChange, 0);
                    PerformLayout();
                    break;

                case Keys.Right:
                    HorizontalScroll.Value += HorizontalScroll.SmallChange;
                    PerformLayout();
                    break;

                case Keys.Up:
                    VerticalScroll.Value =
                        Math.Max(VerticalScroll.Value - VerticalScroll.SmallChange, 0);
                    PerformLayout();
                    break;

                case Keys.Down:
                    VerticalScroll.Value += VerticalScroll.SmallChange;
                    PerformLayout();
                    break;

                case Keys.PageUp:
                    if (Keys.None == ModifierKeys) {
                        VerticalScroll.Value =
                            Math.Max(VerticalScroll.Value - VerticalScroll.LargeChange, 0);
                    } else if (Keys.Shift == ModifierKeys) {
                        HorizontalScroll.Value =
                            Math.Max(HorizontalScroll.Value - HorizontalScroll.LargeChange, 0);
                    }
                    PerformLayout();
                    break;

                case Keys.PageDown:
                    if (Keys.None == ModifierKeys) {
                        VerticalScroll.Value += VerticalScroll.LargeChange;
                    } else if (Keys.Shift == ModifierKeys) {
                        HorizontalScroll.Value += HorizontalScroll.LargeChange;
                    }
                    PerformLayout();
                    break;

                case Keys.Home:
                    if (Keys.None == ModifierKeys) {
                        VerticalScroll.Value = 0;
                    } else if (Keys.Shift == ModifierKeys) {
                        HorizontalScroll.Value = 0;
                    }
                    PerformLayout();
                    break;

                case Keys.End:
                    if (Keys.None == ModifierKeys) {
                        VerticalScroll.Value = VerticalScroll.Maximum;
                    } else if (Keys.Shift == ModifierKeys) {
                        HorizontalScroll.Value = HorizontalScroll.Maximum;
                    }
                    PerformLayout();
                    break;

                case Keys.Back:
                    NavPrev();
                    break;

                default:
                    base.OnKeyDown(e);
                    break;
            }
        }*/

        /*protected override void OnMouseUp(MouseEventArgs e)
        {
            if (MouseButtons.XButton1 == e.Button) {
                NavPrev();
            } else if (MouseButtons.XButton2 == e.Button) {
                NavNext();
            } else {
                base.OnMouseUp(e);
            }
        }*/

        /*protected override void OnMouseWheel(MouseEventArgs e)
        {
            if (Keys.None == ModifierKeys) {
                VerticalScroll.Value = Algorithms.CheckBounds(VerticalScroll.Value - e.Delta, VerticalScroll.Minimum, VerticalScroll.Maximum);
                PerformLayout();
            } else if (Keys.Shift == ModifierKeys) {
                HorizontalScroll.Value = Algorithms.CheckBounds(HorizontalScroll.Value - e.Delta, HorizontalScroll.Minimum, HorizontalScroll.Maximum);
                PerformLayout();
            } else {
                base.OnMouseWheel(e);
            }
        }*/

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
