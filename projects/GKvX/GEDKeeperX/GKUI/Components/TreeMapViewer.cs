/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using BSLib.DataViz.TreeMap;
using GKCore;
using SkiaSharp;
using SkiaSharp.Views.Forms;
using Xamarin.Forms;

namespace GKUI.Components
{
    public class PaintItemEventArgs : EventArgs
    {
        private readonly SKCanvas canvas;
        private readonly MapItem item;

        public SKCanvas Canvas
        {
            get { return canvas; }
        }

        public MapItem Item
        {
            get { return item; }
        }

        public PaintItemEventArgs(SKCanvas canvas, MapItem item)
        {
            if (canvas == null) {
                throw new ArgumentNullException(nameof(canvas));
            }

            this.canvas = canvas;
            this.item = item;
        }
    }

    public delegate void PaintItemEventHandler(object sender, PaintItemEventArgs args);

    /// <summary>
    /// TreeMap Viewer's control.
    /// </summary>
    public class TreeMapViewer : ContentView
    {
        private readonly SKCanvasView fCanvas;
        private SKPaint fBackPaint;
        private SKPaint fBorderPen;
        private MapItem fCurrentItem;
        private SKPaint fDrawFont;
        private Brush fHeaderBrush;
        private int fItemsPadding;
        private TreemapModel fModel;
        private MapItem fRootItem;
        private bool fShowNames;
        private MapItem fUpperItem;

        public MapItem CurrentItem
        {
            get {
                return fCurrentItem;
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

        public event PaintItemEventHandler PaintItem;

        public event EventHandler MouseDoubleClick;


        public TreeMapViewer()
        {
            fCanvas = new SKCanvasView();
            fCanvas.PaintSurface += OnPaint;
            Content = fCanvas;

            fItemsPadding = 4;
            fModel = new TreemapModel();
            fModel.CreatingItem += CreateSimpleItem;
            fDrawFont = new SKPaint(new SKFont(SKTypeface.FromFamilyName("Sans"), 9)) { Color = SKColors.Black };
            fBorderPen = new SKPaint() { Color = SKColors.Black, Style = SKPaintStyle.Stroke };
            //fHeaderBrush = new SolidBrush(SKColors.Black);
            fBackPaint = new SKPaint() { Color = SKColors.Silver, Style = SKPaintStyle.Fill };
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

                    fModel.CalcLayout(itemsList, new MapRect(0, 0, fCanvas.CanvasSize.Width, fCanvas.CanvasSize.Height), headerHeight, fItemsPadding);

                    fCanvas.InvalidateSurface();
                }
            } catch {
            }
        }

        protected override void OnSizeAllocated(double width, double height)
        {
            base.OnSizeAllocated(width, height);
            UpdateView();
        }

        private void OnPaint(object sender, SKPaintSurfaceEventArgs args)
        {
            var info = args.Info;
            var surface = args.Surface;
            var canvas = surface.Canvas;

            canvas.Clear();

            try {
                if (info.Width <= 0 || info.Height <= 0) return;

                var itemsList = GetRootList();
                if (itemsList.Count > 0) {
                    PaintItems(canvas, itemsList);
                } else {
                    canvas.DrawRect(0, 0, info.Width, info.Height, fBackPaint);
                }
            } catch (Exception ex) {
                Logger.WriteError("TreeMapViewer.OnPaint()", ex);
            }
        }

        protected virtual void PaintItems(SKCanvas gfx, IList<MapItem> items)
        {
            int num = items.Count;
            for (int i = 0; i < num; i++) {
                MapItem item = items[i];
                OnPaintItem(gfx, item);
            }
        }

        protected virtual void OnPaintItem(SKCanvas gfx, MapItem item)
        {
            var handler = PaintItem;
            if (handler != null) {
                handler(this, new PaintItemEventArgs(gfx, item));
            } else {
                var rect = item.Bounds;
                if (rect.W > 2 && rect.H > 2) {
                    var skRect = SKRect.Create(rect.X, rect.Y, rect.W, rect.H);
                    gfx.DrawRect(skRect, fBackPaint);
                    gfx.DrawRect(skRect, fBorderPen);
                    gfx.DrawText(item.Name, rect.X, rect.Y, fDrawFont);
                }
            }
            PaintItems(gfx, item.Items);
        }
    }
}
