/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Drawing;
using System.Windows.Forms;
using GKCore;
using GKUI.Platform.Handlers;
using GKUI.Themes;

namespace GKUI.Components
{
    /// <summary>
    /// Accordion-style navigation list: collapsible group headers with items,
    /// only one group is expanded at a time.
    /// </summary>
    public class GKAccordion : ListBox
    {
        private Image fExpandIcon;
        private Image fCollapseIcon;
        private bool fProcessing;
        private int fItemHeight;

        public new int ItemHeight
        {
            get {
                return fItemHeight;
            }
            set {
                fItemHeight = value;
                RefreshItems();
            }
        }

        public GKAccordion()
        {
            DrawMode = DrawMode.OwnerDrawVariable;
            SetStyle(ControlStyles.AllPaintingInWmPaint | ControlStyles.OptimizedDoubleBuffer | ControlStyles.ResizeRedraw, true);
            UpdateStyles();

            BackColor = WFThemeManager.GetThemeColor(ThemeElement.Window);
            fProcessing = false;
            fItemHeight = 30;

            RefreshTheme();
        }

        /// <summary>
        /// Repaints the owner-drawn list, used after theme changes;
        /// also reloads the expand/collapse glyphs for the new theme.
        /// </summary>
        public void RefreshTheme()
        {
            var expandHandler = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_SlideExpand, true) as ImageHandler;
            var collapseHandler = AppHost.ThemeManager.GetThemeImage(ThemeElement.Glyph_SlideCollapse, true) as ImageHandler;

            fExpandIcon = expandHandler?.Handle;
            fCollapseIcon = collapseHandler?.Handle;

            BackColor = WFThemeManager.GetThemeColor(ThemeElement.Window);
            Invalidate();
        }

        #region Public members

        public AccordionItem AddGroup(string text)
        {
            AccordionItem item = new AccordionItem(text, true, null);
            Items.Add(item);
            return item;
        }

        public AccordionItem AddItem(string text)
        {
            AccordionItem item = new AccordionItem(text, false, null);
            Items.Add(item);
            return item;
        }

        public AccordionItem AddItem(string text, object data)
        {
            AccordionItem item = new AccordionItem(text, false, data);
            Items.Add(item);
            return item;
        }

        public void SetItemSelected(int index)
        {
            if (fProcessing) return;
            fProcessing = true;

            AccordionItem item = (AccordionItem)Items[index];

            if (item.IsGroup) {
                ExpandGroupAt(index);
                SelectedIndex = index + 1;
            } else {
                SelectedIndex = index;
            }

            fProcessing = false;
        }

        /// <summary>
        /// Selects the item with the specified data, expanding its group if needed.
        /// </summary>
        public void SelectItem(object data)
        {
            int idx = FindItemIndex(data);
            if (idx < 0) return;

            int grpIdx = FindGroupIndexBefore(idx);
            if (grpIdx >= 0 && !((AccordionItem)Items[grpIdx]).IsExpanded) {
                ExpandGroupAt(grpIdx);
            }

            SetItemSelected(idx);
        }

        #endregion

        #region Private members

        private int FindItemIndex(object data)
        {
            for (int i = 0; i < Items.Count; i++) {
                AccordionItem item = (AccordionItem)Items[i];
                if (!item.IsGroup && item.Data == data) {
                    return i;
                }
            }
            return -1;
        }

        private int FindGroupIndexBefore(int itemIndex)
        {
            for (int i = itemIndex - 1; i >= 0; i--) {
                AccordionItem item = (AccordionItem)Items[i];
                if (item.IsGroup) {
                    return i;
                }
            }
            return -1;
        }

        private void ExpandGroupAt(int index)
        {
            int count = Items.Count;
            SetItemsExpanded(0, count - 1, false);
            SetItemsVisible(0, count - 1, false);

            int nextIdx = count - 1;
            for (int i = index + 1; i < count; i++) {
                AccordionItem itm = (AccordionItem)Items[i];
                if (itm.IsGroup) {
                    nextIdx = i - 1;
                    break;
                }
            }

            ((AccordionItem)Items[index]).IsExpanded = true;
            SetItemsVisible(index + 1, nextIdx, true);

            RefreshItems();
        }

        private void SetItemsVisible(int startIndex, int endIndex, bool val)
        {
            for (int i = startIndex; i <= endIndex; i++) {
                AccordionItem item = (AccordionItem)Items[i];
                if (!item.IsGroup) item.IsVisible = val;
            }
        }

        private void SetItemsExpanded(int startIndex, int endIndex, bool val)
        {
            for (int i = startIndex; i <= endIndex; i++) {
                AccordionItem item = (AccordionItem)Items[i];
                if (item.IsGroup) item.IsExpanded = val;
            }
        }

        private void DrawBoxIcon(Graphics g, Rectangle rect, bool expanded)
        {
            var icon = expanded ? fCollapseIcon : fExpandIcon;

            int dy = (rect.Height - icon.Height) / 2;
            int x = rect.Right - icon.Width - dy;
            int y = rect.Top + dy;

            g.DrawImage(icon, x, y);
        }

        private static void DrawItemText(Graphics g, string text, Font font, Color color, Rectangle rect)
        {
            using (var brush = new SolidBrush(color)) {
                using (var fmt = new StringFormat()) {
                    fmt.Alignment = StringAlignment.Near;
                    fmt.LineAlignment = StringAlignment.Center;
                    g.DrawString(text, font, brush, rect, fmt);
                }
            }
        }

        #endregion

        #region Protected members

        protected override void OnMeasureItem(MeasureItemEventArgs e)
        {
            if (e.Index < 0 || e.Index >= Items.Count) return;

            AccordionItem itm = (AccordionItem)Items[e.Index];

            e.ItemHeight = (!itm.IsVisible) ? 0 : fItemHeight;

            base.OnMeasureItem(e);
        }

        protected override void OnDrawItem(DrawItemEventArgs e)
        {
            if (e.Index < 0 || e.Index >= Items.Count) return;

            AccordionItem item = (AccordionItem)Items[e.Index];
            Rectangle rt = e.Bounds;

            Color clrStrip = WFThemeManager.GetThemeColor(ThemeElement.Strip);
            Color clrWindow = WFThemeManager.GetThemeColor(ThemeElement.Window);
            Color clrText = WFThemeManager.GetThemeColor(ThemeElement.ControlText);
            Color clrSelBack = WFThemeManager.GetThemeColor(ThemeElement.ButtonFace);
            Color clrSelText = WFThemeManager.GetThemeColor(ThemeElement.ButtonText);

            if (item.IsGroup) {
                using (var backBrush = new SolidBrush(clrStrip)) {
                    e.Graphics.FillRectangle(backBrush, rt);
                }

                DrawBoxIcon(e.Graphics, rt, item.IsExpanded);

                rt.Inflate(-5, -2);
                using (var fnt = new Font(Font, FontStyle.Bold)) {
                    DrawItemText(e.Graphics, item.Text, fnt, clrText, rt);
                }
            } else {
                bool selected = ((e.State & DrawItemState.Selected) == DrawItemState.Selected);
                Color backColor = selected ? clrSelBack : clrWindow;
                Color foreColor = selected ? clrSelText : clrText;

                using (var backBrush = new SolidBrush(backColor)) {
                    e.Graphics.FillRectangle(backBrush, rt);
                }

                rt.Inflate(-5, -2);
                DrawItemText(e.Graphics, item.Text, Font, foreColor, rt);
            }

            base.OnDrawItem(e);
        }

        protected override void OnSelectedIndexChanged(EventArgs e)
        {
            int idx = SelectedIndex;
            if (idx >= 0) {
                SetItemSelected(idx);
            }

            base.OnSelectedIndexChanged(e);
        }

        #endregion
    }
}
