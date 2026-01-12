/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System.Windows.Forms;
using GKCore.Design.Graphics;
using GKCore.Design.Controls;

namespace GKUI.Platform.Handlers
{
    public sealed class PictureBoxHandler : BaseControlHandler<PictureBox, PictureBoxHandler>, IPictureBox
    {
        public IImage Image
        {
            get {
                return new ImageHandler(Control.Image);
            }
            set {
                if (value == null) {
                    Control.Image = null;
                } else {
                    var image = ((ImageHandler)value).Handle;
                    Control.Image = image;
                }
            }
        }

        public PictureBoxHandler(PictureBox control) : base(control)
        {
        }
    }
}
