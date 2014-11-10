/* CChild.cs
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
    // Data structure to record the position of the child record in the file.
    // Used to sort children when no birth date is known
    public class CChild
    {
        // Index of child in the family record
        public int m_nPositionInFile;

        // The CIndividualRecord for this child
        public CIndividualRecord m_ir;

        // Constructor
        public CChild( int nPositionInFile, CIndividualRecord ir )
        {
            m_nPositionInFile = nPositionInFile;
            m_ir = ir;
        }

        // Comparer. Compares birth dates if known, otherwise falls back to file position.
        public class Comparer : IComparer
        {
            public int Compare(object x, object y) 
            {
                CIndividualRecord i1 = null;
                CIndividualRecord i2 = null;
                CPGDate date1 = null;
                CPGDate date2 = null;

                if( x != null && x is CChild )
                {
                    i1 = ((CChild)x).m_ir;
                    if( i1 != null )
                    {
                        CPGQualifiedDate date1Qual = i1.BirthDate;
                        if( date1Qual != null )
                        {
                            date1 = date1Qual.m_date;
                        }
                        else
                        {
                            date1 = null;
                        }
                        
                        if( date1 == null )
                        {
                            i1 = null;
                        }
                    }
                }
                if( y != null && y is CChild )
                {
                    i2 = ((CChild)y).m_ir;

                    if( i2 != null )
                    {
                        CPGQualifiedDate date2Qual = i2.BirthDate;
                        if( date2Qual != null )
                        {
                            date2 = date2Qual.m_date;
                        }
                        else
                        {
                            date2 = null;
                        }
                        
                        if( date2 == null )
                        {
                            i2 = null;
                        }
                    }
                }

                if( i1 == null )
                {
                    if( i2 == null )
                    {
                        int p1 = -1;
                        if( x != null )
                        {
                            p1 = ((CChild)x).m_nPositionInFile;
                        }
                        int p2 = -1;
                        if( y != null )
                        {
                            p2 = ((CChild)y).m_nPositionInFile;
                        }
                        if( p1 < p2 )
                        {
                            return -1;
                        }
                        if( p1 > p2 )
                        {
                            return 1;
                        }
                        return 0;
                    }
                    return 1;
                }
                
                if( i2 == null )
                {
                    return -1;
                }

                return date1.CompareTo( date2 );
            }
        }   
    }
}
