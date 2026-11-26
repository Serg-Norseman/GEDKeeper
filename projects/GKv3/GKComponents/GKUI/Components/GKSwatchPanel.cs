/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using Eto.Drawing;
using Eto.Forms;

namespace GKUI.Components
{
    /// <summary>
    /// Bezel-bordered swatch panel with a centered caption, used as a clickable
    /// placeholder for colors and fonts. The caption label exposes its identifier
    /// through <see cref="LabelName"/> so the kernel-side code can resolve it
    /// as <c>ILabel</c> by name (BackColor and Text are set on the label).
    /// </summary>
    public class GKSwatchPanel : Scrollable
    {
        private readonly Label lblText;

        public Label Label
        {
            get { return lblText; }
        }

        public string LabelName
        {
            get { return lblText.ID; }
            set { lblText.ID = value; }
        }

        public Color TextColor
        {
            get { return lblText.TextColor; }
            set { lblText.TextColor = value; }
        }

        public event EventHandler Click;


        public GKSwatchPanel()
        {
            Border = BorderType.Bezel;
            Cursor = Cursors.Pointer;

            lblText = new Label {
                TextAlignment = TextAlignment.Center,
                VerticalAlignment = VerticalAlignment.Center
            };
            lblText.MouseDown += lblText_MouseDown;
            Content = lblText;
        }

        private void lblText_MouseDown(object sender, MouseEventArgs e)
        {
            e.Handled = true;
            Click?.Invoke(this, EventArgs.Empty);
        }
    }
}
