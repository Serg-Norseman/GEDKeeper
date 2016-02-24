/*
 *  "BSLib", Brainstorm Library.
 *  Copyright (C) 2015-2016 by Serg V. Zhdanovskih (aka Alchemist, aka Norseman).
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
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;

namespace BSLib.Controls
{
	public class SlideList : ListBox
	{
		private readonly Bitmap fCollapseIcon;
		private readonly Bitmap fExpandIcon;

        private bool fProcessing;
		private int fItemHeight;

		public new int ItemHeight
		{
			get {
				return this.fItemHeight;
			}
			set {
				this.fItemHeight = value;
				this.RefreshItems();
			}
		}
		
		public SlideList()
		{
			this.DrawMode = DrawMode.OwnerDrawVariable;

			ComponentResourceManager resources = new ComponentResourceManager(typeof(SlideList));
        	fCollapseIcon = (Bitmap)(resources.GetObject("CollapseIcon"));
        	fExpandIcon = (Bitmap)(resources.GetObject("ExpandIcon"));

        	base.DoubleBuffered = true;
        	base.SetStyle(ControlStyles.AllPaintingInWmPaint, true);
        	base.SetStyle(ControlStyles.OptimizedDoubleBuffer, true);
        	base.UpdateStyles();
        	
        	this.fProcessing = false;
        	this.fItemHeight = 30;
		}

		#region Public members

		public SlideListItem AddItem(string text)
        {
            SlideListItem item = new SlideListItem(text);
            this.Items.Add(item);
            return item;
        }

        public SlideListItem AddItem(string text, object data)
        {
            SlideListItem item = new SlideListItem(text, data);
            this.Items.Add(item);
            return item;
        }

        public SlideListItem AddGroupItem(string text, object data)
        {
            SlideListItem item = new SlideListItem(text, true, data);
            this.Items.Add(item);
            return item;
        }

        public SlideListItem InsertItemInGroup(string group, string text, object data)
        {
            SlideListItem item = new SlideListItem(text, false, data);

            bool hasGroup = false;
            
            int num = this.Items.Count;
        	for (int i = 0; i < num; i++) {
                SlideListItem itm = (SlideListItem)this.Items[i];

        		if (itm.IsGroup) {
        			if (itm.Text == group) {
        				hasGroup = true;
        			} else {
        				if (hasGroup) {
        					this.Items.Insert(i, item);
        					return item;
        				}
        			}
        		}
        	}
            
            if (hasGroup) {
            	this.Items.Add(item);
            	return item;
            }

            return null;
        }

        public void SetItemSelected(int index)
        {
            if (this.fProcessing) return;
            this.fProcessing = true;

            SlideListItem item = (SlideListItem)this.Items[index];

        	if (item.IsGroup) {
        		int count = this.Items.Count;
        		SetItemsExpanded(0, count - 1, false);
        		SetItemsVisible(0, count - 1, false);

        		int nextIdx = count - 1;
        		for (int i = index + 1; i < count; i++) {
                    SlideListItem itm = (SlideListItem)this.Items[i];
        			if (itm.IsGroup) {
        				nextIdx = i - 1;
        				break;
        			}
        		}

        		item.IsExpanded = true;
        		SetItemsVisible(index + 1, nextIdx, true);

        		this.RefreshItems();
        		this.SelectedIndex = index + 1;
        	} else {
        		this.SelectedIndex = index;
        	}

            this.fProcessing = false;
        }
        
		#endregion
		
        #region Private members
        
        private void SetItemsVisible(int startIndex, int endIndex, bool val)
        {
        	for (int i = startIndex; i <= endIndex; i++) {
                SlideListItem item = (SlideListItem)this.Items[i];
        		if (!item.IsGroup) item.IsVisible = val;
        	}
        }
        
        private void SetItemsExpanded(int startIndex, int endIndex, bool val)
        {
        	for (int i = startIndex; i <= endIndex; i++) {
                SlideListItem item = (SlideListItem)this.Items[i];
        		if (item.IsGroup) item.IsExpanded = val;
        	}
        }

        #endregion

        #region Protected members
        
        protected override void OnMeasureItem(MeasureItemEventArgs e)
        {
        	if (e.Index < 0 || e.Index >= this.Items.Count) return;
        	
        	SlideListItem itm = (SlideListItem)this.Items[e.Index];

        	e.ItemHeight = (!itm.IsVisible) ? 0 : this.fItemHeight;
        }

        protected override void OnDrawItem(DrawItemEventArgs e)
        {
        	if (e.Index < 0 || e.Index >= this.Items.Count) return;

        	SlideListItem item = (SlideListItem)this.Items[e.Index];
        	Rectangle rt = e.Bounds;
        	
        	if (item.IsGroup) {
        		Brush brush = (item.IsExpanded) ? SystemBrushes.ActiveCaption : SystemBrushes.ActiveBorder;
        		Bitmap icon = (item.IsExpanded) ? fCollapseIcon : fExpandIcon;
        		
        		e.Graphics.FillRectangle(brush, rt);
        		
        		int dy = (rt.Height - icon.Height) / 2;
        		e.Graphics.DrawImage(icon, rt.Right - icon.Width - dy, rt.Top + dy);
        		
        		Rectangle rt1 = rt;
        		rt1.Width -= 1;
        		rt1.Height -= 1;
        		e.Graphics.DrawRectangle(SystemPens.WindowFrame, rt1);
        	}
            else
        	{
        	    Brush sysBrush;
        		sysBrush = ((e.State & DrawItemState.Selected) == DrawItemState.Selected) ? SystemBrushes.GradientActiveCaption : SystemBrushes.Control;
                e.Graphics.FillRectangle(sysBrush, rt);
        	}

        	rt.Inflate(-5, -2);
        	
        	Font fnt = this.Font;
        	if (item.IsGroup) {
        		fnt = new Font(fnt, FontStyle.Bold);
        	}
        	
        	StringFormat fmt = new StringFormat();
        	fmt.Alignment = StringAlignment.Near;
        	fmt.LineAlignment = StringAlignment.Center;
        	e.Graphics.DrawString(this.Items[e.Index].ToString(), fnt, Brushes.Black, rt, fmt);
        }

        protected override void OnSelectedIndexChanged(EventArgs e)
        {
        	int idx = this.SelectedIndex;
        	if (idx >= 0) {
        		this.SetItemSelected(idx);
        	}

        	base.OnSelectedIndexChanged(e);
        }
        
        #endregion
    }

	public class SlideListItem
	{
		public string Text;
		public object Data;
		public bool IsGroup;
		public bool IsVisible = true;
		public bool IsExpanded = false;

		public SlideListItem()
		{
		}

		public SlideListItem(string text) 
		{
			this.Text = text;
			this.Data = null;
			this.IsGroup = false;
		}

		public SlideListItem(string text, object data)
		{
			this.Text = text;
			this.Data = data;
			this.IsGroup = false;
		}

		public SlideListItem(string text, bool isGroup, object data)
		{
			this.Text = text;
			this.Data = data;
			this.IsGroup = isGroup;
		}

		public override string ToString()
		{
			return this.Text;
		}
	}
}
