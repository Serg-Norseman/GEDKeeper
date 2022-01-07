/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2019 by Sergey V. Zhdanovskih.
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

#if !MONO

using System.Windows.Forms;
using NUnit.Extensions.Forms;

namespace GKTests.ControlTesters
{
    public class DataGridViewTester : ControlTester<DataGridView, DataGridViewTester>
    {
        public DataGridViewTester()
        {
        }

        public DataGridViewTester(string name, Form form) : base(name, form)
        {
        }

        public new DataGridView Properties
        {
            get { return (DataGridView) TheObject; }
        }

        public void SelectCell(int row, int col)
        {
            Properties.CurrentCell = Properties.Rows[row].Cells[col];
        }

        public void EnterCell(int row, int col, string value)
        {
            DataGridViewRow dgRow = Properties.Rows[row];
            dgRow.Cells[col].Value = value;
            Properties.CommitEdit(DataGridViewDataErrorContexts.Commit);
            Properties.EndEdit();
        }
    }
}

#endif
