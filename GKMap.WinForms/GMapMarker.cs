/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System;
using System.Drawing;
using System.Windows.Forms;

namespace GKMap.WinForms
{
    /// <summary>
    /// GKMap marker
    /// </summary>
    public abstract class GMapMarker : IDisposable
    {
        private Rectangle fArea;
        private bool fDisposed;
        private bool fIsMouseOver;
        private Point fOffset;
        private GMapOverlay fOverlay;
        private PointLatLng fPosition;
        private string fToolTipText;
        private bool fVisible = true;

        /// <summary>
        /// marker position in local coordinates, internal only, do not set it manually
        /// </summary>
        public Point LocalPosition
        {
            get {
                return fArea.Location;
            }
            set {
                if (fArea.Location != value) {
                    fArea.Location = value;
                    if (Overlay != null && Overlay.Control != null) {
                        if (!Overlay.Control.HoldInvalidation) {
                            Overlay.Control.Invalidate();
                        }
                    }
                }
            }
        }

        public Point Offset
        {
            get {
                return fOffset;
            }
            set {
                if (fOffset != value) {
                    fOffset = value;

                    if (IsVisible) {
                        if (Overlay != null && Overlay.Control != null) {
                            Overlay.Control.UpdateMarkerLocalPosition(this);
                        }
                    }
                }
            }
        }

        public GMapOverlay Overlay
        {
            get {
                return fOverlay;
            }
            internal set {
                fOverlay = value;
            }
        }

        public PointLatLng Position
        {
            get {
                return fPosition;
            }
            set {
                if (fPosition != value) {
                    fPosition = value;

                    if (IsVisible) {
                        if (Overlay != null && Overlay.Control != null) {
                            Overlay.Control.UpdateMarkerLocalPosition(this);
                        }
                    }
                }
            }
        }

        public object Tag;

        /// <summary>
        /// ToolTip position in local coordinates
        /// </summary>
        public Point ToolTipPosition
        {
            get {
                Point ret = fArea.Location;
                ret.Offset(-Offset.X, -Offset.Y);
                return ret;
            }
        }

        public Size Size
        {
            get {
                return fArea.Size;
            }
            set {
                fArea.Size = value;
            }
        }

        public Rectangle LocalArea
        {
            get {
                return fArea;
            }
        }

        public GMapToolTip ToolTip;

        public MarkerTooltipMode ToolTipMode = MarkerTooltipMode.OnMouseOver;

        public string ToolTipText
        {
            get {
                return fToolTipText;
            }
            set {
                if (ToolTip == null && !string.IsNullOrEmpty(value)) {
                    ToolTip = new GMapRoundedToolTip(this);
                }
                fToolTipText = value;
            }
        }

        /// <summary>
        /// is marker visible
        /// </summary>
        public bool IsVisible
        {
            get {
                return fVisible;
            }
            set {
                if (value != fVisible) {
                    fVisible = value;

                    if (Overlay != null && Overlay.Control != null) {
                        if (fVisible) {
                            Overlay.Control.UpdateMarkerLocalPosition(this);
                        } else {
                            if (Overlay.Control.IsMouseOverMarker) {
                                Overlay.Control.IsMouseOverMarker = false;
                                Overlay.Control.RestoreCursorOnLeave();
                            }
                        }

                        if (!Overlay.Control.HoldInvalidation) {
                            Overlay.Control.Invalidate();
                        }
                    }
                }
            }
        }

        /// <summary>
        /// if true, marker will be rendered even if it's outside current view
        /// </summary>
        public bool DisableRegionCheck;

        /// <summary>
        /// can maker receive input
        /// </summary>
        public bool IsHitTestVisible = true;

        /// <summary>
        /// is mouse over marker
        /// </summary>
        public bool IsMouseOver
        {
            get {
                return fIsMouseOver;
            }
            internal set {
                fIsMouseOver = value;
            }
        }

        protected GMapMarker(PointLatLng pos)
        {
            Position = pos;
        }

        public virtual void OnRender(Graphics g)
        {
            //
        }

        public virtual void Dispose()
        {
            if (!fDisposed) {
                fDisposed = true;

                Tag = null;

                if (ToolTip != null) {
                    fToolTipText = null;
                    ToolTip.Dispose();
                    ToolTip = null;
                }
            }
        }
    }

    public delegate void MarkerClick(GMapMarker item, MouseEventArgs e);
    public delegate void MarkerEnter(GMapMarker item);
    public delegate void MarkerLeave(GMapMarker item);
    public delegate void MarkerDoubleClick(GMapMarker item, MouseEventArgs e);

    /// <summary>
    /// mode of tooltip
    /// </summary>
    public enum MarkerTooltipMode
    {
        OnMouseOver,
        Always,
    }
}
