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

using System;
using GDModel;
using GKCore.Interfaces;

namespace GKCore.Operations
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ChangeTracker : UndoManager
    {
        private readonly IBaseContext fBaseContext;

        public IBaseContext Context
        {
            get { return fBaseContext; }
        }

        public ChangeTracker(IBaseContext baseContext)
        {
            fBaseContext = baseContext;
        }

        public bool DoOrdinaryOperation(OperationType type, IGDMObject obj, object newVal)
        {
            if (obj == null)
                throw new ArgumentNullException("obj");

            if (newVal == null)
                throw new ArgumentNullException("newVal");

            return DoOperation(new OrdinaryOperation(this, type, obj, newVal));
        }

        public bool DoIndividualNameChange(GDMIndividualRecord iRec, string surname, string name, string patronymic, string marriedSurname, string nickname)
        {
            if (iRec == null)
                throw new ArgumentNullException("iRec");

            return DoOperation(new IndividualNameChange(this, iRec, surname, name, patronymic, marriedSurname, nickname));
        }
    }
}
