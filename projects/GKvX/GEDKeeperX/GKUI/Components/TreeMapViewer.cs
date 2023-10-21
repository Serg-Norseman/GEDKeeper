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
using BSLib.DataViz.TreeMap;
using SkiaSharp;
using Xamarin.Forms;

namespace GKUI.Components
{
    public class PaintItemEventArgs : EventArgs
    {
        private readonly SKSurface surface;
        private readonly MapItem item;

        public SKSurface Surface
        {
            get { return surface; }
        }

        public MapItem Item
        {
            get { return item; }
        }

        public PaintItemEventArgs(SKSurface surface, MapItem item)
        {
            if (surface == null) {
                throw new ArgumentNullException("surface");
            }

            this.surface = surface;
            this.item = item;
        }
    }

    public delegate void PaintItemEventHandler(object sender, PaintItemEventArgs args);

    public class TreeMapViewer : ContentView
    {
        private MapItem fCurrentItem;
        private string fHint;
        private MapItem fHoveredItem;
        private int fItemsPadding;
        private TreemapModel fModel;
        private bool fMouseoverHighlight;
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

        public event EventHandler MouseDoubleClick;


        public TreeMapViewer()
        {
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
        }
    }
}
