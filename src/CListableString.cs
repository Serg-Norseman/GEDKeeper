/* CListableString.cs
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
using System.Windows.Forms;

namespace GEDmill
{
    // Special class of ListViewSubItem that sorts strings numerically even if they start with alphabetic character.
    public class CListableString : ListViewItem.ListViewSubItem
    {
        // The string we are encapsulating
        protected string m_string;

        // Constructor
        public CListableString( string s )
        {
            m_string = s;
            base.Text = m_string;
        }

        // To display list
        public override string ToString()
        {
            return m_string;
        }

        // Special compare will sort numerically if string is formated like "AAA111" or "I124".
        // Return -1 if this instance is less than other...
        public int CompareTo(CListableString other) 
        {
            int nLeftL = m_string.Length;
            int nRightL = other.m_string.Length;
            for( int i = 0; i < nLeftL; ++i )
            {   
                if( i >= nRightL )
                {
                    // left string is longer than right. right is greater.
                    return 1;
                }
                char cLeft = m_string[i];
                char cRight = other.m_string[i];
                if( Char.IsDigit( cLeft ) && Char.IsDigit( cRight ) )
                {
                    // Compare rest of strings numerically
                    int nLeft = 0;
                    int nRight = 0;
                    string sLeft = m_string.Substring( i );
                    string sRight = other.m_string.Substring( i );
                    bool bLeftIsNumeric = true;
                    bool bRightIsNumeric = true;
                    foreach( char c in sLeft )
                    {
                        if( !Char.IsDigit( c ) )
                        {
                            bLeftIsNumeric = false;
                            break;
                        }
                        nLeft *= 10;
                        nLeft += c - '0';
                    }
                    if( bLeftIsNumeric )
                    {
                        foreach( char c in sRight )
                        {
                            if( !Char.IsDigit( c ) )
                            {
                                bRightIsNumeric = false;
                                break;
                            }
                            nRight *= 10;
                            nRight += c - '0';
                        }
                        if( bRightIsNumeric )
                        {
                            return nLeft - nRight;
                        }
                    }
                }
                if( cLeft < cRight )
                {
                    return -1;
                }
                else if( cLeft > cRight )
                {
                    return 1;
                }

            }
            return m_string.CompareTo( other.m_string );
        }
    }
}
