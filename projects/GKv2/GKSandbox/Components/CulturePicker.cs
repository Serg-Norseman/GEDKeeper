/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017 by Sergey V. Zhdanovskih.
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
using System.Collections;
using System.Drawing;
using System.Globalization;
using System.Windows.Forms;
using BSLib.Controls;
using GKSandbox;

namespace GKIntl
{
    public class CulturePicker : DropDownControl
    {
        private System.ComponentModel.IContainer components;
        private ListView fListView;
        private ImageList fImageList;

        public event EventHandler CultureChanged;

        public string CultureName
        {
            get { return fListView.SelectedItems[0].Text; }
        }

        public CulturePicker()
        {
            InitializeComponent();

            InitializeDropDown(fListView);

            var resourceManager = GKIFlags.ResourceManager;
            var resourceSet = resourceManager.GetResourceSet(CultureInfo.InvariantCulture, true, true);
            foreach (DictionaryEntry entry in resourceSet)
            {
                fImageList.Images.Add(entry.Key.ToString(), (Image)entry.Value);
            }

            foreach (CultureInfo ci in CultureInfo.GetCultures(CultureTypes.AllCultures))
            {
                try {
                    var ri = new RegionInfo(ci.LCID);
                    string regName = ri.EnglishName.Replace(' ', '-');

                    var item = new ListViewItem(ci.DisplayName + ", " + regName);
                    item.ImageKey = regName;

                    fListView.Items.Add(item);
                } catch (Exception) {
                    // dummy
                }
            }
        }

        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        private void InitializeComponent()
        {
            components = new System.ComponentModel.Container();
            SuspendLayout();

            fImageList = new ImageList(components);
            fImageList.ColorDepth = ColorDepth.Depth8Bit;
            fImageList.ImageSize = new Size(24, 24);
            fImageList.TransparentColor = Color.Transparent;

            fListView = new ListView();
            fListView.LargeImageList = fImageList;
            fListView.Location = new Point(0, 30);
            fListView.Margin = new Padding(4, 4, 4, 4);
            fListView.MultiSelect = false;
            fListView.Name = "fListView";
            fListView.Size = new Size(243, 181);
            fListView.TabIndex = 0;
            fListView.UseCompatibleStateImageBehavior = false;
            fListView.View = View.Tile;
            fListView.SelectedIndexChanged += listView1_SelectedIndexChanged;

            AnchorSize = new Size(244, 21);
            AutoScaleDimensions = new SizeF(8F, 16F);
            AutoScaleMode = AutoScaleMode.Font;
            Controls.Add(fListView);
            Margin = new Padding(4, 4, 4, 4);
            Name = "CulturePicker";
            Size = new Size(244, 215);
            ResumeLayout(false);
        }

        private void listView1_SelectedIndexChanged(object sender, EventArgs e)
        {
            if (DropState == DropDownState.Dropping || DropState == DropDownState.Closing) return;
            if (fListView.SelectedItems.Count == 0) return;

            Text = fListView.SelectedItems[0].Text;
            CloseDropDown();

            if (CultureChanged != null)
                CultureChanged(null, null);
        }

        protected override void OnResize(EventArgs e)
        {
            base.OnResize(e);
            fListView.TileSize = new Size(Width, 28);
        }
    }
}
