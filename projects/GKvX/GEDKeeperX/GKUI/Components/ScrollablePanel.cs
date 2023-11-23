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
using BSLib;
using SkiaSharp;
using SkiaSharp.Views.Forms;
using Xamarin.Forms;

namespace GKUI.Components
{
    public class MouseEventArgs : EventArgs
    {
        //public Keys Modifiers { get; private set; }

        public SKMouseButton Buttons { get; private set; }

        public Point Location { get; private set; }

        public bool Handled { get; set; }

        public float Pressure { get; private set; }

        public int Delta { get; private set; }

        public MouseEventArgs(SKMouseButton buttons/*, Keys modifiers*/, Point location, int delta = 0, float pressure = 1f)
        {
            Buttons = buttons;
            //Modifiers = modifiers;
            Location = location;
            Delta = delta;
            Pressure = pressure;
        }
    }


    public class ZoomEventArgs : EventArgs
    {
        public double Scale { get; } = 1.0;

        public Point ScaleOrigin { get; }

        public GestureStatus Status { get; }

        public ZoomEventArgs(GestureStatus status, double scale, Point origin)
        {
            ScaleOrigin = origin;
            Scale = scale;
        }
    }


    /// <summary>
    ///
    /// </summary>
    public class ScrollablePanel : ScrollView
    {
        public const int SmallChange = 1;
        public const int LargeChange = 10;

        private SKCanvasView fCanvas;
        private bool fCenteredImage;
        private Font fFont;
        private bool fHasHScroll;
        private bool fHasVScroll;
        private ExtSize fImageSize;
        private Rectangle fImageRect;
        private Rectangle fImageViewport;
        private int fMouseOffsetX, fMouseOffsetY;
        private Color fTextColor;
        private ExtRect fViewport;


        public Rectangle CanvasRectangle
        {
            get { return fCanvas.Bounds; }
        }

        protected bool CenteredImage
        {
            get { return fCenteredImage; }
            set { fCenteredImage = value; }
        }

        /// <summary>
        /// Gets the rectangle that represents the client area of the control.
        /// </summary>
        public Rectangle ClientRectangle
        {
            get {
                // FIXME: bad!!!
                //var clientSize = base.ClientSize;
                var clientSize = base.ContentSize;
                return new Rectangle(0, 0, clientSize.Width, clientSize.Height);
            }
        }

        public Font Font
        {
            get { return fFont; }
            set {
                if (fFont != value) {
                    fFont = value;
                    OnFontChanged(EventArgs.Empty);
                }
            }
        }

        protected Rectangle ImageRect
        {
            get { return fImageRect; }
        }

        protected ExtRect ImageViewport
        {
            get { return UIHelper.Rt2Rt(fImageViewport); }
        }

        public Point MouseOffset
        {
            get {
                return new Point(fMouseOffsetX, fMouseOffsetY);
            }
        }

        public bool HScroll
        {
            get { return fHasHScroll; }
        }

        public bool VScroll
        {
            get { return fHasVScroll; }
        }

        public Color TextColor
        {
            get { return fTextColor; }
            set { fTextColor = value; }
        }

        /// <summary>
        /// The rectangle that is visible to the user.
        /// </summary>
        public ExtRect Viewport
        {
            get { return fViewport; }
        }


        public ScrollablePanel()
        {
            base.Padding = new Thickness(0);

            fCanvas = new SKCanvasView();
            fCanvas.IgnorePixelScaling = true;
            fCanvas.PaintSurface += PaintHandler;
            fCanvas.EnableTouchEvents = true;
            fCanvas.Touch += OnTouch;
            fCanvas.VerticalOptions = LayoutOptions.CenterAndExpand;
            fCanvas.HorizontalOptions = LayoutOptions.CenterAndExpand;
            //fCanvas.SizeChanged += OnSizeChanged;
            Content = fCanvas;

            base.Orientation = ScrollOrientation.Both;
            base.SizeChanged += OnSizeChanged;
            base.Scrolled += OnScroll;

            var pinch = new PinchGestureRecognizer();
            pinch.PinchUpdated += OnPinchUpdated;
            fCanvas.GestureRecognizers.Add(pinch);

            var tap = new TapGestureRecognizer { NumberOfTapsRequired = 2 };
            tap.Tapped += OnTapped;
            fCanvas.GestureRecognizers.Add(tap);

            //fFont = SystemFonts.Label();
            //fTextColor = SystemColors.ControlText;

            SetImageSize(new ExtSize(100, 100), false);
        }

        protected virtual void OnFontChanged(EventArgs e)
        {
        }

        private void PaintHandler(object sender, SKPaintSurfaceEventArgs e)
        {
            e.Surface.Canvas.Clear();
            OnPaint(e);
        }

        protected virtual void OnPaint(SKPaintSurfaceEventArgs e)
        {
        }

        private void UpdateProperties()
        {
            if (fViewport.IsEmpty()) return;

            fHasHScroll = (fViewport.Width < fImageSize.Width);
            fHasVScroll = (fViewport.Height < fImageSize.Height);

            int destX, destY;

            if (fHasHScroll) {
                destX = 0;
                fMouseOffsetX = fViewport.Left;
            } else {
                if (fCenteredImage) {
                    destX = (fViewport.Width - fImageSize.Width) / 2;
                    fMouseOffsetX = -destX;
                } else {
                    destX = 0;
                    fMouseOffsetX = 0;
                }
            }

            if (fHasVScroll) {
                destY = 0;
                fMouseOffsetY = fViewport.Top;
            } else {
                if (fCenteredImage) {
                    destY = (fViewport.Height - fImageSize.Height) / 2;
                    fMouseOffsetY = -destY;
                } else {
                    destY = 0;
                    fMouseOffsetY = 0;
                }
            }

            fImageRect = new Rectangle(destX, destY, fImageSize.Width, fImageSize.Height);

            int width = Math.Min(fImageSize.Width, fViewport.Width);
            int height = Math.Min(fImageSize.Height, fViewport.Height);
            fImageViewport = new Rectangle(destX, destY, width, height);
        }

        /*protected override void OnMouseDown(MouseEventArgs e)
        {
            if (!HasFocus) {
                Focus();
            }

            e.Handled = true;
            base.OnMouseDown(e);
        }

        protected override void OnMouseWheel(MouseEventArgs e)
        {
            //Console.WriteLine("ScrollablePanel.OnMouseWheel()");

            int delta = -(int)(e.Delta.Height * 120.0f);

            if (Keys.None == e.Modifiers) {
                AdjustScroll(0, delta);
            } else if (Keys.Shift == e.Modifiers) {
                AdjustScroll(delta, 0);
            }

            e.Handled = true;
            base.OnMouseWheel(e);
        }

        protected override void OnShown(EventArgs e)
        {
            if (Loaded) {
                try {
                    fViewport = VisibleRect;
                    UpdateProperties();
                } catch {
                    // FIXME: works in MacOS and doesn't work in Wpf
                }
            }

            base.OnShown(e);
        }*/

        private void OnSizeChanged(object sender, EventArgs e)
        {
            fViewport = new ExtRect((int)ScrollX, (int)ScrollY, (int)Width, (int)Height);
            UpdateProperties();
        }

        private void OnScroll(object sender, ScrolledEventArgs e)
        {
            fViewport = new ExtRect((int)ScrollX, (int)ScrollY, (int)Width, (int)Height);
            UpdateProperties();
            fCanvas.InvalidateSurface();
        }

        /// <summary>
        /// Updates the scroll position.
        /// </summary>
        /// <param name="posX">The X position.</param>
        /// <param name="posY">The Y position.</param>
        protected void UpdateScrollPosition(int posX, int posY)
        {
            SetScrolledPosition(posX, posY);
        }

        /// <summary>
        /// Adjusts the scroll.
        /// </summary>
        /// <param name="dx">The X shift.</param>
        /// <param name="dy">The Y shift.</param>
        protected void AdjustScroll(int dx, int dy)
        {
            UpdateScrollPosition((int)ScrollX + dx, (int)ScrollY + dy);
        }

        /// <summary>
        /// Sets the sizes of canvas.
        /// </summary>
        /// <param name="imageSize">The size of canvas.</param>
        /// <param name="noRedraw">Flag of the need to redraw.</param>
        protected void SetImageSize(ExtSize imageSize, bool noRedraw = false)
        {
            if (!imageSize.IsEmpty) {
                fImageSize = imageSize;

                ExtSize clientSize = new ExtSize(fViewport.Width, fViewport.Height);
                int canvWidth = Math.Max(imageSize.Width, clientSize.Width);
                int canvHeight = Math.Max(imageSize.Height, clientSize.Height);
                //fCanvas.Size = new Size(canvWidth, canvHeight);
                fCanvas.WidthRequest = canvWidth;
                fCanvas.HeightRequest = canvHeight;

                //base.UpdateScrollSizes();
                UpdateProperties();
            }

            if (!noRedraw) InvalidateContent();
        }

        protected Point GetImageRelativeLocation(Point mpt, bool buttons)
        {
            Point pt = mpt;
            /*if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux) && buttons) {
                int aX = (int)mpt.X - fImageViewport.X;
                int aY = (int)mpt.Y - fImageViewport.Y;

                pt = new Point(aX, aY);
            } else {
                pt = new Point((int)mpt.X + fMouseOffsetX, (int)mpt.Y + fMouseOffsetY);
            }*/
            return pt;
        }

        protected Point GetControlRelativeLocation(Point mpt, bool buttons)
        {
            Point pt = mpt;
            /*if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux) && buttons) {
                int aX = (int)mpt.X - ((!fHasHScroll) ? 0 : fMouseOffsetX);
                int aY = (int)mpt.Y - ((!fHasVScroll) ? 0 : fMouseOffsetY);

                pt = new PointF(aX, aY);
            } else {
                pt = new PointF(mpt.X, mpt.Y);
            }*/
            return pt;
        }

        protected void InvalidateContent()
        {
            fCanvas.InvalidateSurface();
        }

        private void OnTapped(object sender, EventArgs e)
        {
            OnMouseDoubleClick(e);
        }

        protected virtual void OnMouseDoubleClick(EventArgs e)
        {
        }

        protected virtual void OnMouseDown(MouseEventArgs e)
        {
        }

        protected virtual void OnMouseUp(MouseEventArgs e)
        {
        }

        protected virtual void OnMouseMove(MouseEventArgs e)
        {
        }

        protected virtual void OnMouseWheel(MouseEventArgs e)
        {
        }

        /*private static int fClickCount;

        private bool ClickHandle()
        {
            if (fClickCount > 1) {
                // double tap
                OnMouseDoubleClick(EventArgs.Empty);
            } else {
                // single tap
            }
            fClickCount = 0;
            return false;
        }*/

        private void OnTouch(object sender, SKTouchEventArgs e)
        {
            var xPt = new Point(e.Location.X, e.Location.Y);
            var mouseArgs = new MouseEventArgs(e.MouseButton, xPt, e.WheelDelta, e.Pressure);

            switch (e.ActionType) {
                case SKTouchAction.Pressed: {
                        if (!IsFocused) base.Focus();
                        OnMouseDown(mouseArgs);

                        /*if (fClickCount < 1) {
                            TimeSpan tt = new TimeSpan(0, 0, 0, 0, 250);
                            Device.StartTimer(tt, ClickHandle);
                        }
                        fClickCount++;*/
                    }
                    break;

                case SKTouchAction.Released:
                    OnMouseUp(mouseArgs);
                    break;

                case SKTouchAction.Moved:
                    OnMouseMove(mouseArgs);
                    break;

                case SKTouchAction.WheelChanged:
                    OnMouseWheel(mouseArgs);
                    break;
            }

            e.Handled = mouseArgs.Handled;
        }

        protected virtual void OnZoom(ZoomEventArgs e)
        {
        }

        private void OnPinchUpdated(object sender, PinchGestureUpdatedEventArgs e)
        {
            var args = new ZoomEventArgs(e.Status, e.Scale, e.ScaleOrigin);
            OnZoom(args);

            switch (e.Status) {
                case GestureStatus.Started:
                    /*LastScale = e.Scale;
                    StartScale = Scale;
                    AnchorX = e.ScaleOrigin.X;
                    AnchorY = e.ScaleOrigin.Y;*/
                    break;

                case GestureStatus.Running:
                    /*if (e.Scale < 0 || Math.Abs(LastScale - e.Scale) > (LastScale * 1.3) - LastScale) { return; }
                    LastScale = e.Scale;
                    var current = Scale + (e.Scale - 1) * StartScale;
                    Scale = Clamp(current, MIN_SCALE * (1 - OVERSHOOT), MAX_SCALE * (1 + OVERSHOOT));*/
                    break;

                case GestureStatus.Completed:
                    /*if (Scale > MAX_SCALE)
                        this.ScaleTo(MAX_SCALE, 250, Easing.SpringOut);
                    else if (Scale < MIN_SCALE)
                        this.ScaleTo(MIN_SCALE, 250, Easing.SpringOut);*/
                    break;
            }
        }
    }
}
