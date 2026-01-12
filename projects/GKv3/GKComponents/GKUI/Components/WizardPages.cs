/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
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
