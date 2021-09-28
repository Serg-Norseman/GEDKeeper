/*
 *  This file is part of the "GKMap".
 *  GKMap project borrowed from GMap.NET (by radioman).
 *
 *  Copyright (C) 2009-2018 by radioman (email@radioman.lt).
 *  This program is licensed under the FLAT EARTH License.
 */

using System.Drawing;
using System.Windows.Forms;

namespace GKMap.WinForms
{
    /// <summary>
    /// GKMap marker
    /// </summary>
    public abstract class GMapMarker : GMapObject
    {
        private Rectangle fArea;
        private Point fOffset;
        private PointLatLng fPosition;
        private string fToolTipText;

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

        public Size Size
        {
            get {
                return fArea.Size;
            }
            set {
                fArea.Size = value;
            }
        }

        public GMapToolTip ToolTip { get; set; }

        public MarkerTooltipMode ToolTipMode { get; set; }

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


        protected GMapMarker(PointLatLng pos)
        {
            Position = pos;
            fVisible = true;
            ToolTipMode = MarkerTooltipMode.OnMouseOver;
            IsHitTestVisible = true;
        }

        internal override bool IsInside(int x, int y)
        {
            return fArea.Contains(x, y);
        }

        protected override void UpdateLocalPosition()
        {
            if (fVisible) {
                Overlay.Control.UpdateMarkerLocalPosition(this);
            } else {
                if (Overlay.Control.IsMouseOverMarker) {
                    Overlay.Control.IsMouseOverMarker = false;
                    Overlay.Control.RestoreCursorOnLeave();
                }
            }
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
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
