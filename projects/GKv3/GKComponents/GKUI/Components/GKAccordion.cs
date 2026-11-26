/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using System.Collections.Generic;
using Eto.Drawing;
using Eto.Forms;
using GKCore;
using GKUI.Platform.Handlers;
using GKUI.Themes;

namespace GKUI.Components
{
    /// <summary>
    /// Accordion-style navigation list: collapsible group headers with items,
    /// only one group is expanded at a time. Based on the old SlideList prototype.
    /// </summary>
    public class GKAccordion : Scrollable
    {
        private sealed class AccordionCanvas : Drawable
        {
            private readonly GKAccordion fOwner;


            public AccordionCanvas(GKAccordion owner)
            {
                fOwner = owner;
            }

            protected override void OnPaint(PaintEventArgs e)
            {
                base.OnPaint(e);
                fOwner.PaintCanvas(e.Graphics, Width);
            }

            protected override void OnMouseDown(MouseEventArgs e)
            {
                base.OnMouseDown(e);
                fOwner.HandleClick((int)e.Location.Y);
            }
        }

        private const int GroupTextIndent = 8;
        private const int ItemTextIndent = 24;

        private readonly List<AccordionItem> fItems;
        private readonly AccordionCanvas fCanvas;
        private readonly Font fItemFont;
        private readonly Font fGroupFont;
        private Image fExpandIcon;
        private Image fCollapseIcon;
        private int fItemHeight;
        private AccordionItem fSelectedItem;

        public event EventHandler SelectedItemChanged;


        public GKAccordion()
        {
            fItems = new List<AccordionItem>();
            fItemHeight = 30;
            fItemFont = SystemFonts.Default();
            fGroupFont = SystemFonts.Bold();

            // stretch the canvas to the viewport width, so there are no unpainted
            // areas left outside of it during splitter resizes
            ExpandContentWidth = true;

            fCanvas = new AccordionCanvas(this);
            Content = fCanvas;

            SizeChanged += Canvas_SizeChanged;
            RefreshTheme();
        }

        public int ItemHeight
        {
            get { return fItemHeight; }
            set {
                fItemHeight = value;
                UpdateSize();
            }
        }

        /// <summary>
        /// Repaints the owner-drawn canvas, used after theme changes;
        /// also reloads the expand/collapse glyphs for the new theme.
        /// </summary>
        public void RefreshTheme()
        {
            fExpandIcon = GetThemeGlyph(ThemeElement.Glyph_SlideExpand);
            fCollapseIcon = GetThemeGlyph(ThemeElement.Glyph_SlideCollapse);
            fCanvas.Invalidate();
        }

        private static Image GetThemeGlyph(ThemeElement element)
        {
            var themeImage = AppHost.ThemeManager.GetThemeImage(element, true) as ImageHandler;
            return themeImage?.Handle;
        }

        public AccordionItem SelectedItem
        {
            get { return fSelectedItem; }
        }

        public AccordionItem AddGroup(string text)
        {
            var item = new AccordionItem(text, true, null);
            fItems.Add(item);
            UpdateSize();
            return item;
        }

        public AccordionItem AddItem(string text, object data)
        {
            var item = new AccordionItem(text, false, data);
            fItems.Add(item);
            UpdateSize();
            return item;
        }

        /// <summary>
        /// Selects the item with the specified data, expanding its group if needed.
        /// </summary>
        public void SelectItem(object data)
        {
            AccordionItem target = null;

            foreach (var item in fItems) {
                if (!item.IsGroup && item.Data == data) {
                    target = item;
                    break;
                }
            }

            if (target == null) return;

            AccordionItem group = null;

            for (int i = fItems.IndexOf(target) - 1; i >= 0; i--) {
                if (fItems[i].IsGroup) {
                    group = fItems[i];
                    break;
                }
            }

            if (group != null && !group.IsExpanded) {
                ExpandGroup(group, target);
                return;
            }

            SelectItem(target);
        }

        private void Canvas_SizeChanged(object sender, EventArgs e)
        {
            UpdateSize();
        }

        private void UpdateSize()
        {
            int numVisible = 0;

            foreach (var item in fItems) {
                if (item.IsGroup || item.IsVisible) numVisible++;
            }

            // only the height is fixed here; the width is stretched to the
            // viewport by ExpandContentWidth, so the canvas covers all the
            // visible area and gets repainted without stale artifacts
            fCanvas.Height = numVisible * fItemHeight;
            fCanvas.Invalidate();
        }

        private void SelectItem(AccordionItem item)
        {
            if (item == null || item.IsGroup) return;

            if (fSelectedItem != item) {
                fSelectedItem = item;
                SelectedItemChanged?.Invoke(this, EventArgs.Empty);
            }
            fCanvas.Invalidate();
        }

        private void ExpandGroup(AccordionItem group, AccordionItem selectTarget)
        {
            foreach (var item in fItems) {
                if (item.IsGroup) {
                    item.IsExpanded = false;
                } else {
                    item.IsVisible = false;
                }
            }

            int idx = fItems.IndexOf(group);
            int nextIdx = fItems.Count - 1;

            for (int i = idx + 1; i < fItems.Count; i++) {
                if (fItems[i].IsGroup) {
                    nextIdx = i - 1;
                    break;
                }
            }

            group.IsExpanded = true;

            for (int i = idx + 1; i <= nextIdx; i++) {
                fItems[i].IsVisible = true;
            }

            UpdateSize();

            AccordionItem selItem = selectTarget;
            if (selItem == null && nextIdx > idx && !fItems[idx + 1].IsGroup) {
                selItem = fItems[idx + 1];
            }

            if (selItem != null) {
                SelectItem(selItem);
            } else {
                fCanvas.Invalidate();
            }
        }

        internal void HandleClick(int y)
        {
            int row = y / fItemHeight;
            int visibleRow = 0;

            foreach (var item in fItems) {
                if (!item.IsGroup && !item.IsVisible) continue;

                if (visibleRow == row) {
                    if (item.IsGroup) {
                        ExpandGroup(item, null);
                    } else {
                        SelectItem(item);
                    }
                    return;
                }

                visibleRow++;
            }
        }

        internal void PaintCanvas(Graphics g, int width)
        {
            Color groupColor = EtoThemeManager.GetThemeColor(ThemeElement.Control);
            Color groupTextColor = EtoThemeManager.GetThemeColor(ThemeElement.ControlText);
            Color itemColor = EtoThemeManager.GetThemeColor(ThemeElement.Window);
            Color itemTextColor = EtoThemeManager.GetThemeColor(ThemeElement.ControlText);
            Color selColor = EtoThemeManager.GetThemeColor(ThemeElement.ButtonFace);
            Color selTextColor = EtoThemeManager.GetThemeColor(ThemeElement.ButtonText);

            int y = 0;

            foreach (var item in fItems) {
                if (!item.IsGroup && !item.IsVisible) continue;

                var rect = new Rectangle(0, y, width, fItemHeight);

                if (item.IsGroup) {
                    g.FillRectangle(groupColor, rect);
                    DrawTextVCentered(g, fGroupFont, groupTextColor, GroupTextIndent, rect, item.Text);
                    DrawBoxIcon(g, rect, item.IsExpanded);
                } else {
                    bool selected = (item == fSelectedItem);
                    Color backColor = selected ? selColor : itemColor;
                    Color foreColor = selected ? selTextColor : itemTextColor;

                    g.FillRectangle(backColor, rect);
                    DrawTextVCentered(g, fItemFont, foreColor, ItemTextIndent, rect, item.Text);
                }

                y += fItemHeight;
            }
        }

        private static void DrawTextVCentered(Graphics g, Font font, Color color, int indent, Rectangle rect, string text)
        {
            SizeF textSize = g.MeasureString(font, text);
            float ty = rect.Y + (rect.Height - textSize.Height) / 2;
            g.DrawText(font, color, indent, ty, text);
        }

        private void DrawBoxIcon(Graphics g, Rectangle rect, bool expanded)
        {
            var icon = expanded ? fCollapseIcon : fExpandIcon;

            int dy = (rect.Height - icon.Height) / 2;
            int x = rect.Right - icon.Width - dy;
            int y = rect.Y + dy;

            g.DrawImage(icon, x, y);
        }
    }
}
