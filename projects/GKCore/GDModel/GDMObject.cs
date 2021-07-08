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

using System;

namespace GDModel
{
    /// <summary>
    /// Base class for all GEDCOM objects like tags, and tree and xref replacer.
    /// 
    /// Because descendants of this class will never interact with unmanaged objects
    /// and to save memory (these objects have a very large number of instances)
    /// support for the IDisposable interface has been cut. But to eliminate possible
    /// collisions in the future, it was left in a commented out form.
    /// </summary>
    public class GDMObject : IDisposable, IGDMObject
    {
        /*private bool fDisposed;*/

        protected virtual void Dispose(bool disposing)
        {
        }

        public void Dispose()
        {
            /*if (!fDisposed) {
                Dispose(true);
                fDisposed = true;
            }
            GC.SuppressFinalize(this);*/

            Dispose(true);
        }

        ~GDMObject()
        {
            Dispose(false);
        }
    }
}
