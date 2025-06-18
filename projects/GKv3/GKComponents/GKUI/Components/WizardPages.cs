/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2025 by Sergey V. Zhdanovskih.
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
using System.Collections.ObjectModel;
using System.Linq;
using Eto;
using Eto.Forms;

namespace GKUI.Components
{
    public class WizardPage : Panel
    {

    }

    /// <summary>
    /// Analogue of TabControl (or Notebook component) with support for hiding tabs.
    /// </summary>
    [ContentProperty("Pages")]
    public class WizardPages : Panel
    {
        private class WizardPageCollection : Collection<WizardPage>
        {
            private readonly WizardPages control;

            internal WizardPageCollection(WizardPages control)
            {
                this.control = control;
            }
        }


        private int fCurrentPageIndex;
        private WizardPageCollection fPages;
        private Panel fPagePlaceholder;


        public override IEnumerable<Control> Controls
        {
            get {
                IEnumerable<Control> enumerable = fPages;
                return enumerable ?? Enumerable.Empty<Control>();
            }
        }

        public Collection<WizardPage> Pages => fPages ?? (fPages = new WizardPageCollection(this));

        public int SelectedIndex
        {
            get { return fCurrentPageIndex; }
            set {
                fCurrentPageIndex = value;
                UpdateCurrentPage();
            }
        }

        public WizardPage SelectedPage
        {
            get {
                if (SelectedIndex >= 0) {
                    return Pages[SelectedIndex];
                }
                return null;
            }
            set {
                SelectedIndex = fPages.IndexOf(value);
            }
        }


        public WizardPages()
        {
            fCurrentPageIndex = 0;
            fPagePlaceholder = new Panel();
            Content = fPagePlaceholder;
        }

        public override void Remove(Control child)
        {
            if (child is WizardPage item) {
                Pages.Remove(item);
            }
        }

        private void UpdateCurrentPage()
        {
            var page = fPages[fCurrentPageIndex];
            if (fPagePlaceholder.Content != page) {
                fPagePlaceholder.Content = page;
            }
        }
    }
}
