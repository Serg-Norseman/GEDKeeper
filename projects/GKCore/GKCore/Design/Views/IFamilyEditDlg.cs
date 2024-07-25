/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2024 by Sergey V. Zhdanovskih.
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

using GDModel;
using GKCore.Design.Controls;
using GKCore.Interfaces;
using GKCore.Lists;
using GKCore.Types;

namespace GKCore.Design.Views
{
    public interface IFamilyEditDlg : ICommonDialog, IBaseEditor
    {
        GDMFamilyRecord FamilyRecord { get; set; }

        void SetTarget(TargetMode targetType, GDMIndividualRecord target);
        void LockEditor(bool locked);
        void SetHusband(string value);
        void SetWife(string value);

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
