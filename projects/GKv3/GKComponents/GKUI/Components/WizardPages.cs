/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2022 by Sergey V. Zhdanovskih.
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

using System.Collections.Generic;
using Eto.Forms;

namespace GKUI.Components
{
    public class WizardPage : Panel
    {

    }

    /// <summary>
    /// Analogue of TabControl (or Notebook component) with support for hiding tabs.
    /// </summary>
    public class WizardPages : Container
    {
        private IList<WizardPage> fPages;


        public override IEnumerable<Control> Controls
        {
            get {
                return fPages;
            }
        }

        public IList<WizardPage> Pages
        {
            get { return fPages; }
        }


        public WizardPages()
        {
            // TODO: not implemented yet
        }

        public override void Remove(Control child)
        {
            var page = child as WizardPage;
            if (page != null) {
                fPages.Remove(page);
            }
        }
    }
}
