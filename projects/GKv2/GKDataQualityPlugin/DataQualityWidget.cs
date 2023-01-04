/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2017-2022 by Sergey V. Zhdanovskih.
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
using System.Drawing;
using System.Windows.Forms;
using BSLib.DataViz.TreeMap;
using GDModel;
using GKCore;
using GKCore.Interfaces;
using GKCore.Tools;

namespace GKDataQualityPlugin
{
    public partial class DataQualityWidget : Form, ILocalizable
    {
        private class GroupMapItem : MapItem
        {
            public Color Color;
            public List<GDMRecord> GroupRecords;

            public GroupMapItem(MapItem parent, string name, double size) : base(parent, name, size)
            {
                Color = Color.Silver;
            }
        }


        private TreeMapViewer fDataMap;
        private GroupMapItem fCurrentItem;
        private readonly Plugin fPlugin;
        private IBaseWindow fBase;
        private ContextMenuStrip fContextMenu;


        public DataQualityWidget(Plugin plugin)
        {
            InitializeComponent();

            var miRefresh = new ToolStripMenuItem();
            miRefresh.Text = plugin.LangMan.LS(CLS.LSID_Refresh);
            miRefresh.Click += miRefresh_Click;

            var miResetFilter = new ToolStripMenuItem();
            miResetFilter.Text = plugin.LangMan.LS(CLS.LSID_ResetFilter);
            miResetFilter.Click += miResetFilter_Click;

            fContextMenu = new ContextMenuStrip(this.components);
            fContextMenu.Items.AddRange(new ToolStripItem[] {
                miRefresh,
                new ToolStripSeparator(),
                miResetFilter
            });

            fPlugin = plugin;
            fDataMap = new TreeMapViewer();
            fDataMap.Model.CreatingItem += DataMap_CreateGroupItem;
            fDataMap.Dock = DockStyle.Fill;
            fDataMap.MouseDoubleClick += DataMap_DoubleClick;
            fDataMap.PaintItem += DataMap_PaintItem;
            fDataMap.ContextMenuStrip = fContextMenu;
            Controls.Add(fDataMap);

            SetLocale();
        }

        private void miResetFilter_Click(object sender, EventArgs e)
        {
            SetExternalFilter(null);
        }

        private void miRefresh_Click(object sender, EventArgs e)
        {
            UpdateTreeMap();
        }

        private GroupMapItem CreateItem(MapItem parent, string name, double size, float quality, List<GDMRecord> groupRecords)
        {
            var item = fDataMap.Model.CreateItem(parent, name, size) as GroupMapItem;
            item.GroupRecords = groupRecords;

            double wavelength = Spectrum.ColdWavelength + (Spectrum.WavelengthMaximum - Spectrum.ColdWavelength) * (1.0f - quality);
            item.Color = Spectrum.WavelengthToRGB(wavelength);

            return item;
        }

        private void DataQualityWidget_Load(object sender, EventArgs e)
        {
            fPlugin.Host.WidgetShow(fPlugin);
            BaseChanged(fPlugin.Host.GetCurrentFile());
        }

        private void DataQualityWidget_Closed(object sender, EventArgs e)
        {
            SetExternalFilter(null);
            BaseChanged(null);
            fPlugin.Host.WidgetClose(fPlugin);
        }

        public void BaseChanged(IBaseWindow baseWin)
        {
            if (fBase != baseWin) {
                fBase = baseWin;
                UpdateTreeMap();
            }
        }

        private void UpdateTreeMap()
        {
            fDataMap.Model.Items.Clear();
            if (fBase == null) {
                fDataMap.Invalidate();
                return;
            }

            string hint = fPlugin.LangMan.LS(CLS.LSID_Hint);

            GDMTree tree = fBase.Context.Tree;
            List<GDMIndividualRecord> prepared = new List<GDMIndividualRecord>();
            try {
                int groupNum = 0;
                int num = tree.RecordsCount;
                for (int i = 0; i < num; i++) {
                    GDMRecord rec = tree[i];

                    if (rec.RecordType == GDMRecordType.rtIndividual) {
                        GDMIndividualRecord iRec = (GDMIndividualRecord)rec;
                        if (!prepared.Contains(iRec)) {
                            groupNum++;
                            var groupRecords = new List<GDMRecord>();

                            TreeTools.WalkTree(tree, iRec, TreeTools.TreeWalkMode.twmAll, groupRecords);

                            int groupSize = groupRecords.Count;
                            float quality = 0.0f;
                            for (int j = 0; j < groupSize; j++) {
                                iRec = (GDMIndividualRecord)groupRecords[j];
                                prepared.Add(iRec);

                                quality += GKUtils.GetCertaintyAssessment(iRec);
                            }
                            quality /= groupSize;

                            string name = string.Format(hint, groupNum, groupSize, quality.ToString("0.00"));

                            CreateItem(null, name, groupSize, quality, groupRecords);
                        }
                    }
                }
            } finally {
            }

            fDataMap.UpdateView();
        }

        private MapItem DataMap_CreateGroupItem(MapItem parent, string name, double size)
        {
            return new GroupMapItem(parent, name, size);
        }

        private void DataMap_PaintItem(object sender, PaintItemEventArgs args)
        {
            var gfx = args.Graphics;
            var item = args.Item;
            MapRect bounds = item.Bounds;
            if (bounds.W > 2f && bounds.H > 2f) {
                var simpleItem = (GroupMapItem)item;
                gfx.FillRectangle(new SolidBrush(simpleItem.Color), bounds.X, bounds.Y, bounds.W, bounds.H);
                gfx.DrawRectangle(fDataMap.BorderPen, bounds.X, bounds.Y, bounds.W, bounds.H);

                var rect = new RectangleF(bounds.X, bounds.Y, bounds.W, bounds.H);
                gfx.DrawString(simpleItem.Name, Font, fDataMap.HeaderBrush, rect);
            }
        }

        private void DataMap_DoubleClick(object sender, MouseEventArgs e)
        {
            fCurrentItem = fDataMap.Model.FindByCoord(e.X, e.Y) as GroupMapItem;
            if (fCurrentItem != null) {
                SetExternalFilter(GroupFilterHandler);
            }
        }

        private void SetExternalFilter(ExternalFilterHandler handler)
        {
            if (fBase != null) {
                var listMan = fBase.GetRecordsListManByType(GDMRecordType.rtIndividual);
                listMan.ExternalFilter = handler;
                fBase.ApplyFilter(GDMRecordType.rtIndividual);
            }
        }

        private bool GroupFilterHandler(GDMRecord record)
        {
            return (fCurrentItem != null) && fCurrentItem.GroupRecords.Contains(record);
        }

        #region ILocalizable support

        public void SetLocale()
        {
            Text = fPlugin.LangMan.LS(CLS.LSID_Title);
        }

        #endregion
    }
}
