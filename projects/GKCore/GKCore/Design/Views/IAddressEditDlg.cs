/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using GDModel;
using GKCore.Design.Controls;
using GKCore.Lists;

namespace GKCore.Design.Views
{
    public interface IAddressEditDlg : ICommonDialog
    {
        GDMAddress Address { get; set; }

        ITextBox Country { get; }
        ITextBox State { get; }
        ITextBox City { get; }
        ITextBox PostalCode { get; }
        ITextBox AddressLine { get; }

        ISheetList PhonesList { get; }
        ISheetList MailsList { get; }
        ISheetList WebsList { get; }
    }
}
