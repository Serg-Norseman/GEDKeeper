/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using System.Xml.Linq;
using BSLib.DataViz.TreeMap;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Components
{
    public class PaintItemEventArgs : EventArgs
    {
        private Graphics graphics;
        private readonly MapItem item;

        public Graphics Graphics
        {
            get { return graphics; }
        }

        public MapItem Item
        {
            get { return item; }
        }

        public PaintItemEventArgs(Graphics graphics, MapItem item)
        {
            if (graphics == null) {
                throw new ArgumentNullException(nameof(graphics));
            }

            this.graphics = graphics;
            this.item = item;
        }
    }

    public delegate void PaintItemEventHandler(object sender, PaintItemEventArgs args);

    /// <summary>
    /// TreeMap Viewer's control.
    /// </summary>
    public class TreeMapViewer : Drawable
    {
        //private readonly ToolTip fToolTip;

        //private Bitmap fBackBuffer;
        private Pen fBorderPen;
        private MapItem fCurrentItem;
        private Font fDrawFont;
        private Brush fHeaderBrush;
        private Color fHighlightColor;
        private Pen fHighlightPen;
        private string fHint;
        private MapItem fHoveredItem;
        private int fItemsPadding;
        private TreemapModel fModel;
        private bool fMouseoverHighlight;
        private MapItem fRootItem;
        private bool fShowNames;
        private MapItem fUpperItem;


        public Font DrawFont
        {
            get { return fDrawFont; }
            set {
                if (fDrawFont != value) {
                    fDrawFont = value;
                    UpdateView();
                }
            }
        }

        public Pen BorderPen
        {
            get { return fBorderPen; }
            set {
                if (fBorderPen != value) {
                    fBorderPen = value;
                    UpdateView();
                }
            }
        }

        public MapItem CurrentItem
        {
            get {
                return fCurrentItem;
            }
        }

        public Brush HeaderBrush
        {
            get { return fHeaderBrush; }
            set {
                if (fHeaderBrush != value) {
                    fHeaderBrush = value;
                    UpdateView();
                }
            }
        }

        public int ItemsPadding
        {
            get {
                return fItemsPadding;
            }
            set {
                if (fItemsPadding != value) {
                    fItemsPadding = value;
                    UpdateView();
                }
            }
        }

        public TreemapModel Model
        {
            get {
                return fModel;
            }
            set {
                fModel = value;
                UpdateView();
            }
        }

        public bool MouseoverHighlight
        {
            get {
                return fMouseoverHighlight;
            }
            set {
                if (fMouseoverHighlight != value) {
                    fMouseoverHighlight = value;
                    UpdateView();
                }
            }
        }

        public MapItem RootItem
        {
            get { return fRootItem; }
            set {
                if (fRootItem != value) {
                    fRootItem = value;
                    UpdateView();
                }
            }
        }

        public bool ShowNames
        {
            get {
                return fShowNames;
            }
            set {
                if (fShowNames != value) {
                    fShowNames = value;
                    UpdateView();
                }
            }
        }

        public MapItem UpperItem
        {
            get {
                return fUpperItem;
            }
        }

        public event TMHintRequestEventHandler HintRequest;

        public event PaintItemEventHandler PaintItem;


        public TreeMapViewer()
        {
            //base.DoubleBuffered = true;
            //base.SetStyle(ControlStyles.AllPaintingInWmPaint, true);
            //base.SetStyle(ControlStyles.OptimizedDoubleBuffer, true);

            fItemsPadding = 4;
            fModel = new TreemapModel();
            fModel.CreatingItem += CreateSimpleItem;

            /*fToolTip = new ToolTip();
            fToolTip.AutoPopDelay = 5000;
            fToolTip.InitialDelay = 250;
            fToolTip.ReshowDelay = 50;
            fToolTip.ShowAlways = true;*/

#if OS_MSWIN
            fDrawFont = new Font("Calibri", 9);
#else
            fDrawFont = new Font(FontFamilies.SansFamilyName, 9);
#endif

            //fBackBuffer = null;
            fBorderPen = new Pen(Colors.Black);
            fHeaderBrush = new SolidBrush(Colors.Black);
            fHighlightColor = Colors.White;
            fHighlightPen = new Pen(fHighlightColor);
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing) {
                fDrawFont.Dispose();
            }
            base.Dispose(disposing);
        }

        private MapItem CreateSimpleItem(MapItem parent, string name, double size)
        {
            return new MapItem(parent, name, size);
        }

        private List<MapItem> GetRootList()
        {
            return (fRootItem == null) ? fModel.Items : fRootItem.Items;
        }

        public void UpdateView()
        {
            try {
                if (Width != 0 && Height != 0) {
                    List<MapItem> itemsList = GetRootList();
                    int headerHeight = /*(fShowNames) ? Font.Height :*/ fItemsPadding;

                    fModel.CalcLayout(itemsList, new MapRect(0, 0, Width, Height), headerHeight, fItemsPadding);

                    //fBackBuffer = null;
                    Invalidate();
                }
            } catch {
            }
        }

        protected virtual string OnHintRequest(MapItem mapItem)
        {
            var hintRequest = HintRequest;
            if (hintRequest == null) return mapItem.Name;

            TMHintRequestEventArgs args = new TMHintRequestEventArgs(mapItem);
            hintRequest(this, args);
            return args.Hint;
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            Point mpt = new Point(e.Location);

            List<MapItem> itemsList = GetRootList();

            string hint = "";
            fUpperItem = null;
            fCurrentItem = fModel.FindByCoord(itemsList, mpt.X, mpt.Y, out fUpperItem);
            if (fCurrentItem != null) {
                hint = OnHintRequest(fCurrentItem);
            }

            if (fHint != hint) {
                fHint = hint;
                if (string.IsNullOrEmpty(fHint)) {
                    //fToolTip.Hide(this);
                    ToolTip = string.Empty;
                } else {
                    //fToolTip.Show(hint, this, e.X, e.Y, 3000);
                    ToolTip = hint;
                }
            }

            if (fMouseoverHighlight) {
                fHoveredItem = fCurrentItem;
                Invalidate();
            }

            base.OnMouseMove(e);
        }

        protected override void OnSizeChanged(EventArgs e)
        {
            base.OnSizeChanged(e);
            UpdateView();
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            base.OnPaint(e);

            Graphics gfx = e.Graphics;

            /*if (fBackBuffer == null) {
                fBackBuffer = new Bitmap(Width, Height, PixelFormat.Format24bppRgb);

                using (var backGfx = Graphics.FromImage(fBackBuffer)) {
                    List<MapItem> itemsList = GetRootList();
                    if (itemsList.Count > 0) {
                        PaintItems(backGfx, itemsList);
                    } else {
                        backGfx.FillRectangle(new SolidBrush(Colors.Silver), 0, 0, Width, Height);
                    }
                }
            }*/

            List<MapItem> itemsList = GetRootList();
            if (itemsList.Count > 0) {
                PaintItems(gfx, itemsList);
            } else {
                gfx.FillRectangle(new SolidBrush(Colors.Silver), 0, 0, Width, Height);
            }

            //gfx.DrawImage(fBackBuffer, 0, 0);

            if (fMouseoverHighlight) {
                if (fHoveredItem != null) {
                    var rect = fHoveredItem.Bounds;
                    gfx.DrawRectangle(fHighlightPen, rect.X, rect.Y, rect.W, rect.H);
                }
            }
        }

        protected virtual void PaintItems(Graphics gfx, IList<MapItem> items)
        {
            int num = items.Count;
            for (int i = 0; i < num; i++) {
                MapItem item = items[i];
                OnPaintItem(gfx, item);
            }
        }

        protected RectangleF ToRectangle(MapRect rect)
        {
            return new RectangleF(rect.X, rect.Y, rect.W, rect.H);
        }

        protected virtual void OnPaintItem(Graphics gfx, MapItem item)
        {
            var handler = PaintItem;
            if (handler != null) {
                handler(this, new PaintItemEventArgs(gfx, item));
            } else {
                var rect = item.Bounds;
                if (rect.W > 2 && rect.H > 2) {
                    var simpleItem = (MapItem)item;
                    gfx.FillRectangle(new SolidBrush(Colors.Silver), rect.X, rect.Y, rect.W, rect.H);
                    gfx.DrawRectangle(fBorderPen, rect.X, rect.Y, rect.W, rect.H);
                    //gfx.DrawString(simpleItem.Name, Font, fHeaderBrush, ToRectangle(rect));
                    gfx.DrawText(fDrawFont, new SolidBrush(Colors.Black), rect.X, rect.Y, simpleItem.Name);
                }
            }

            PaintItems(gfx, item.Items);
        }
    }
}
