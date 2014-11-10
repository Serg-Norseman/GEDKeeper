/* CISRecord.cs
 * 
 * Copyright 2009 Alexander Curtis <alex@logicmill.com>
 * This file is part of GEDmill - A family history website creator
 * 
 * GEDmill is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * GEDmill is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GEDmill.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * History:  
 * 10Dec08 AlexC          Migrated from GEDmill 1.10
 *
 */

using System;
using System.Collections;

namespace GEDmill.LLClasses
{
    // Individual/Source record base class. See also CISRecordChanges.
    public class CISRecord : GEDmill.LLClasses.CRecord
    {
        // GEDCOM notes for this record
        public ArrayList m_alNoteStructures;

        // GEDCOM multimedia links
        public ArrayList m_alMultimediaLinks;

        // Whether the user has deemed to hide this individual from the website output.
        private bool m_bRestricted;

        // Used to store extra CMultimediaFileReference s added by user.
        public ArrayList m_alUniqueFileRefs; 

        // Constructor
        public CISRecord( CGedcom gedcom ) : base( gedcom )
        {
            m_alNoteStructures = new ArrayList();
            m_alMultimediaLinks = new ArrayList();
            m_bRestricted = false;
            m_alUniqueFileRefs = new ArrayList();
        }

        // User selected restriction (from prune screen)
        public bool Restricted
        {
            set
            {
                m_bRestricted = value;
            }
            get
            {
                return m_bRestricted;
            }
        }


        // Returns a string contains CRLFs describing this record for use in a sMessage box.
        // Overriden by INDI and SOUR classes.
        public virtual string Details()
        {
            return "CISRecord: " + m_xref;
        }

        // Displayed in sources/individuals list
        public int CountVisibleMFRs()
        {
            int nVisible = 0;
            foreach( CMultimediaFileReference mfr in m_alUniqueFileRefs )
            {
                if( mfr.m_bVisible )
                {
                    nVisible++;
                }
            }
            return nVisible;
        }

        // Displayed in sources/individuals list
        public int CountAllMFRs()
        {
            return m_alUniqueFileRefs.Count;
        }

        // Used to remove all pictures for this record from the website
        public int SetAllMFRsVisible( bool bVisible )
        {
            int nChanged = 0;
            foreach( CMultimediaFileReference mfr in m_alUniqueFileRefs )
            {
                if( mfr.m_bVisible != bVisible )
                {
                    mfr.m_bVisible = bVisible;
                    nChanged++;
                }
            }
            return nChanged;
        }
    }
}
