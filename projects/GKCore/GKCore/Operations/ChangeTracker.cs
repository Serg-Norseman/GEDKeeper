/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;
using GDModel;

namespace GKCore.Operations
{
    /// <summary>
    /// 
    /// </summary>
    public sealed class ChangeTracker : UndoManager
    {
        private readonly BaseContext fBaseContext;

        public BaseContext Context
        {
            get { return fBaseContext; }
        }

        public ChangeTracker(BaseContext baseContext)
        {
            fBaseContext = baseContext;
        }

        public bool DoOrdinaryOperation(OperationType type, IGDMObject obj, object newVal)
        {
            if (obj == null)
                throw new ArgumentNullException(nameof(obj));

            if (newVal == null)
                throw new ArgumentNullException(nameof(newVal));

            return DoOperation(new OrdinaryOperation(this, type, obj, newVal));
        }

        public bool DoIndividualNameChange(GDMIndividualRecord iRec, string surname, string name, string patronymic, string marriedSurname, string nickname)
        {
            if (iRec == null)
                throw new ArgumentNullException(nameof(iRec));

            return DoOperation(new IndividualNameChange(this, iRec, surname, name, patronymic, marriedSurname, nickname));
        }
    }
}
