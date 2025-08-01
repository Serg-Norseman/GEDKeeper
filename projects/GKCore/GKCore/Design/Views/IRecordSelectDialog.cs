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

using GDModel;
using GKCore.Design.Controls;

namespace GKCore.Design.Views
{
    public enum TargetMode
    {
        tmNone,
        tmParent,       // select child for parent
        tmChild,        // select parent for child
        tmSpouse,       // select spouse for indi
        tmFamilyChild,  // select family for child
        tmFamilySpouse, // select family with spouse for indi
    }


    public sealed class Target
    {
        public GDMIndividualRecord TargetIndividual;
        public TargetMode TargetMode;
        public GDMSex NeedSex;

        public Target()
        {
        }
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IRecordSelectDialog : ICommonDialog, IBaseEditor
    {
        GDMRecord ResultRecord { get; set; }

        IComboBox FilterCombo { get; }
        ITextBox FilterText { get; }

        IFilterControl FilterCtl { get; }
        IListView RecordsList { get; }

        void SetTarget(TargetMode mode, GDMIndividualRecord target, GDMSex needSex, string defFilter = "*");
    }
}
