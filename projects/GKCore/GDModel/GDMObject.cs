/*
 *  GEDKeeper, the personal genealogical database editor.
 *  Copyright (C) 2009-2026 by Sergey V. Zhdanovskih.
 *
 *  Licensed under the GNU General Public License (GPL) v3.
 *  See LICENSE file in the project root for full license information.
 */

using System;

namespace GDModel
{
    /// <summary>
    /// Base class for all GEDCOM objects like tags, and tree and xref replacer.
    /// 
    /// Because descendants of this class will never interact with unmanaged objects
    /// and to save memory (these objects have a very large number of instances)
    /// support for the IDisposable has been cut. But to eliminate possible
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
