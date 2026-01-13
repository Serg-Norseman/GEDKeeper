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
    public interface IFamilyEditDlg : ICommonDialog
    {
        GDMFamilyRecord FamilyRecord { get; set; }

        void SetTarget(TargetMode targetType, GDMIndividualRecord target);

        ISheetList NotesList { get; }
        ISheetList MediaList { get; }
        ISheetList SourcesList { get; }
        ISheetList ChildrenList { get; }
        ISheetList EventsList { get; }
        ISheetList UserRefList { get; }

        IComboBox MarriageStatus { get; }
        IComboBox Restriction { get; }
        ITextBox Husband { get; }
        ITextBox Wife { get; }
    }
}
