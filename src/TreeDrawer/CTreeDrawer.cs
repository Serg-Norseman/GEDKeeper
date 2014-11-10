/* CTreeDrawer.cs
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
using GEDmill.LLClasses;
using System.Drawing;
using System.Collections;
using System.Drawing.Imaging;

namespace GEDmill.ImageClasses
{
    // In this file, Parents are the top generation, siblings are the middle generation (including the subject of the tree), and children are the bottom generation. Subject's spouses come in middle generation.
    // The data structure looks like this:
/*
 *    ________   _____________________________________________________________________________________________________   ________
 *   | father | | siblings                                                                                            | | mother |
 *   |        |-|  _______   _______   ________   _________________   ______   _________________   _______   _______  |-|        |
 *   |________| | |sibling| |sibling| |spouse/ | |children         | |spouse| |children         | |subject| |sibling| | |________|
 *              | |       |-|       |-|subject |-|  _____   _____  |-|      |-|  _____   _____  |-|/spouse|-|       | |
 *              | |_______| |_______| |________| | |child| |child| | |______| | |child| |child| | |_______| |_______| |
 *              |                                | |_____|-|_____|--------------|_____|-|_____| |                     |
 *              |                                |_________________|          |_________________|                     |
 *              |_____________________________________________________________________________________________________|
 */
    // Class that calculates and draws a mini tree diagram
    public class CTreeDrawer
    {
        // Data structure containing the information to put in the boxes in the tree
        private class CBoxText
        {
            // Individual's name
            public string m_sName;

            // Dates to put in the box
            public string m_sDate;

            // Individual's first name
            public string m_sFirstName;

            // Individual's surname
            public string m_sSurname;

            // Whether the information is private
            public bool m_bConcealed;

            // Constructor
            public CBoxText(CIndividualRecord ir)
            {
                m_sFirstName = "";
                m_sSurname = "";
                m_bConcealed = (ir.Visibility() == CIndividualRecord.EVisibility.Restricted);
                if (m_bConcealed && !MainForm.s_config.m_bUseWithheldNames)
                {
                    m_sFirstName = "";
                    m_sSurname = m_sName = MainForm.s_config.m_sConcealedName;
                }
                else
                {
                    if (ir.Name != "")
                    {
                        m_sName = MainForm.s_config.CapitaliseName(ir.Name, ref m_sFirstName, ref m_sSurname);
                    }
                    else
                    {
                        m_sFirstName = "";
                        m_sSurname = m_sName = MainForm.s_config.m_sUnknownName;
                    }
                }

                if (m_bConcealed)
                {
                    m_sDate = "";
                }
                else
                {
                    m_sDate = ir.LifeYears;
                }
            }
        }

        // Total size of the tree
        protected SizeF m_sizeTotal;

        // Reference to the global gedcom data
        protected CGedcom m_gedcom;

        // Constructor
        public CTreeDrawer(CGedcom gedcom)
        {
            m_gedcom = gedcom;
        }

        // This is the main tree drawing method.
        // irSubject is the individual for whom the tree is based. 
        // nTargeWidth is the width below which the layout is free to use up space to produce a nice tree.
        public ArrayList CreateMiniTree( CPaintbox paintbox, CIndividualRecord ir, string sFilename, int nTargetWidth, System.Drawing.Imaging.ImageFormat imageFormat )
        {
            // First calculate size required for tree, by iterating through individuals and building a data structure
            CMiniTreeGroup mtgParent = CreateDataStructure(ir);

            // For each individual calculate size of box required for display using helper function
            // There must be a better way to get a graphics:
            Bitmap bmp = new Bitmap( 1, 1, System.Drawing.Imaging.PixelFormat.Format24bppRgb );
            Graphics g = Graphics.FromImage( bmp );            
            Font f = paintbox.m_font;

            // Record what font windows actually used, in case it chose a different one
            MainForm.s_config.m_sTreeFontName = f.Name;
            MainForm.s_config.m_fTreeFontSize = f.Size;

            // Recursively calculate sizes of other groups
            mtgParent.CalculateSize( g, f ); 

            g.Dispose();
            bmp.Dispose();

            // Now calculate sizes of each row
            // Total width includes irSubject, their spouses and their siblings.
            // Total height is always three generations

            // Now calculate how best to position each generation
            // Calculate the width of each generation
            // There are three cases : frParents widest, siblings widest, children widest
            // Plus two aims : minimise total width, get offspring centred under frParents.
            // If nTargetWidth is exceeded simply because of number of individuals in one row, that 
            // row's width becomes the new target width.
            // If nTargetWidth is exceeded otherwise, minimising total width becomes the priority
            mtgParent.CalculateLayout( 0f, 0f );
            mtgParent.Compress();

            RectangleF rect = mtgParent.GetExtent();
            m_sizeTotal = new SizeF( rect.Width, rect.Height );
            mtgParent.Translate( -rect.Left, -rect.Top );

            // Calculate offset for each row
            // Can't do this so create a new bitmap: bmp.Width = totalSize.Width;
            // Can't do this so create a new bitmap: bmp.Height = totalSize.Height;
            int nTotalWidth = (int)(m_sizeTotal.Width+1.0f);
            int nTotalHeight = (int)(m_sizeTotal.Height+1.0f);
            bmp = new Bitmap( nTotalWidth, nTotalHeight, System.Drawing.Imaging.PixelFormat.Format32bppArgb );
            g = Graphics.FromImage( bmp );

            // Do background fill
            if( MainForm.s_config.m_bFakeMiniTreeTransparency && paintbox.m_brushFakeTransparency != null )
            {
                g.FillRectangle( paintbox.m_brushFakeTransparency, 0, 0, nTotalWidth, nTotalHeight );
            }
            else if( imageFormat == ImageFormat.Gif && paintbox.m_brushBgGif != null )
            {
                g.FillRectangle( paintbox.m_brushBgGif, 0, 0, nTotalWidth, nTotalHeight );
            }

            ArrayList alMap = new ArrayList();
            mtgParent.DrawBitmap( paintbox, g, alMap );
            
            // Save the bitmap
            LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Note, "Saving mini tree as " + sFilename );

            if( System.IO.File.Exists( sFilename ) )
            {
                // Delete any current file
                System.IO.File.SetAttributes( sFilename, System.IO.FileAttributes.Normal );
                System.IO.File.Delete( sFilename );
            }

            // Save using FileStream to try to avoid crash (only seen by customers)
            System.IO.FileStream fs = new System.IO.FileStream( sFilename, System.IO.FileMode.Create );
            bmp.Save( fs, imageFormat );
            fs.Close();
            
            g.Dispose();
            bmp.Dispose();

            // For gifs we need to reload and set transparency colour
            if( imageFormat == ImageFormat.Gif && !MainForm.s_config.m_bFakeMiniTreeTransparency )
            {
                Image imageGif;
                ColorPalette colorpalette;
                imageGif = Image.FromFile( sFilename );
                colorpalette = imageGif.Palette;

                // Creates a new GIF image with a modified colour palette
                if (colorpalette != null )
                {
                    // Create a new 8 bit per pixel image
                    Bitmap bm=new Bitmap(imageGif.Width,imageGif.Height,PixelFormat.Format8bppIndexed);

                    // Get it's palette
                    ColorPalette colorpaletteNew = bm.Palette;

                    // Copy all the entries from the old palette removing any transparency
                    int n=0;
                    foreach(Color c in colorpalette.Entries)
                    {
                        colorpaletteNew.Entries[n++]=Color.FromArgb(255,c);         
                    }

                    // Now to copy the actual bitmap data
                    // Lock the source and destination bits
                    BitmapData src = ((Bitmap)imageGif).LockBits(new Rectangle(0,0,imageGif.Width,imageGif.Height),ImageLockMode.ReadOnly,imageGif.PixelFormat);
                    BitmapData dst = bm.LockBits(new Rectangle(0,0,bm.Width,bm.Height),ImageLockMode.WriteOnly,bm.PixelFormat);
 
                    // Uses pointers so we need unsafe code.
                    // The project is also compiled with /unsafe
                    byte backColor = 0;
                    unsafe  
                    {
                        backColor = ((byte *)src.Scan0.ToPointer())[0]; // Assume transparent colour appears as first pixel.

                        byte* src_ptr = ((byte *)src.Scan0.ToPointer());
                        byte* dst_ptr = ((byte *)dst.Scan0.ToPointer());
                        // May be useful: System.Runtime.InteropServices.Marshal.Copy(IntPtr source, byte[], destination, int start, int length)
                        // May be useful: System.IO.MemoryStream ms = new System.IO.MemoryStream(src_ptr);
                        int width = imageGif.Width;
                        int src_stride = src.Stride - width;
                        int dst_stride = dst.Stride - width;
                        for(int y=0;y<imageGif.Height;y++)
                        {
                            // Can't convert IntPtr to byte[]: Buffer.BlockCopy( src_ptr, 0, dst_ptr, 0, width );
                            int x = width;
                            while( x-- > 0 )
                            {
                                *dst_ptr++ = *src_ptr++;
                            }
                            src_ptr += src_stride;
                            dst_ptr += dst_stride;
                        }
                    }

                    // Set the newly selected transparency
                    colorpaletteNew.Entries[(int)backColor]=Color.FromArgb(0,Color.Magenta);

                    // Re-insert the palette
                    bm.Palette=colorpaletteNew;

                    // All done, unlock the bitmaps
                    ((Bitmap)imageGif).UnlockBits(src);
                    bm.UnlockBits(dst);

                    imageGif.Dispose();

                    // Set the new image in place
                    imageGif=bm;
                    colorpalette=imageGif.Palette;

                    LogFile.TheLogFile.WriteLine( LogFile.DT_HTML, LogFile.EDebugLevel.Note, "Re-saving mini gif as " + sFilename );

                    imageGif.Save( sFilename, imageFormat );
                }
            }

            return alMap;
        }

        // Calculate size required for tree by iterating through individuals and building a data structure.
        protected CMiniTreeGroup CreateDataStructure( CIndividualRecord irSubject )
        {
            // Add subject's frParents
            CFamilyRecord frParents = m_gedcom.GetFamilyByChild(irSubject, 0);
            CMiniTreeGroup mtgParents = new CMiniTreeGroup();
            CMiniTreeIndividual mtiFather = null;
            if( frParents != null )
            {
                mtiFather = AddToGroup(frParents.m_xrefHusband, mtgParents);
            }
                
            // Create a group for the subejct and their siblings.
            CMiniTreeGroup mtgSiblings = new CMiniTreeGroup();

            // Keeps count of subject's siblings (including subject)
            int nSiblings = 0;

            // Keeps track of last added sibling, to hook up to next added sibling.
            CMiniTreeIndividual mtiRightmostSibling = null;

            // Keeps track of last added child, to hook up to next added child.
            CMiniTreeIndividual mtiRightmostChild = null;

            // For each sibling (including the subject)
            for(;;)
            {
                CIndividualRecord irSibling = GetChild(frParents, nSiblings, irSubject);
                if (null == irSibling)
                {
                    break;
                }
                
                if (irSibling == irSubject)
                {
                    // Add spouses and children of subject, (and subject too, if we need to put wife after them.)
                    CMiniTreeGroup mtgOffspring = null;
                    bool bAddedSubject = false;
                    int nSpouses = 0;
                    CMiniTreeGroup.ECrossbar ecbCrossbar = CMiniTreeGroup.ECrossbar.Solid;
                    ArrayList alFamily = m_gedcom.GetFamilyArray(irSubject);

                    foreach( CFamilyRecord fr in alFamily )
                    {
                        CIndividualRecord irSpouse = fr.GetSpouse(irSubject);

                        if (!fr.IsHusband(irSubject))
                        {
                            mtiRightmostSibling = AddToGroup(irSpouse, mtgSiblings);
                            // Subject is female so all but last husband have dotted bars
                            ecbCrossbar = CMiniTreeGroup.ECrossbar.DottedLeft; 
                        }
                        else if (Exists(irSubject) && !bAddedSubject)
                        {
                            // Subject is male, so need to put them in now, before their children.
                            // (Otherwise they get added as a regular sibling later)
                            CBoxText boxtext = new CBoxText(irSubject);
                            mtiRightmostSibling = mtgSiblings.AddIndividual(irSubject, boxtext.m_sFirstName, boxtext.m_sSurname, boxtext.m_sDate, false, frParents != null, true, boxtext.m_bConcealed, false);
                            
                            // To stop subject being added as regular sibling.
                            bAddedSubject = true; 
                        }

                        int nGrandchildren = 0;
                        CIndividualRecord irGrandchild = null;

                        // If we have already added an offspring box (from previous marriage) need connect this box to it as its right box.
                        if (mtgOffspring != null)
                        {
                            mtgOffspring.RightBox = mtiRightmostSibling;
                        }

                        // Create a box for the offspring of this marriage
                        mtgOffspring = new CMiniTreeGroup();

                        // Set crossbar that joins subject to spouse according to whether this is subject's first spouse.
                        mtgOffspring.m_ecCrossbar = ecbCrossbar;

                        // Add children by this spouse                   
                        CMiniTreeIndividual mtiChild = null;
                        while ((irGrandchild = GetChild(fr,nGrandchildren,null)) != null)
                        {
                            if( Exists(irGrandchild) )
                            {
                                CBoxText boxtext = new CBoxText(irGrandchild);
                                mtiChild = mtgOffspring.AddIndividual(irGrandchild, boxtext.m_sFirstName, boxtext.m_sSurname, boxtext.m_sDate, true, true, false, boxtext.m_bConcealed, false);

                                // Hook this up to any children by previous spouses.
                                if( nGrandchildren==0 && mtiRightmostChild != null )
                                {
                                    mtiRightmostChild.RightObjectAlien = mtiChild;
                                    mtiChild.LeftObjectAlien = mtiRightmostChild;
                                }
                            }
                            nGrandchildren++;
                        }

                        // If we added anything, record it as the right-most child ready to hook to children by next spouse.
                        if( mtiChild != null )
                        {
                            mtiRightmostChild = mtiChild;
                        }

                        // Add the subjects children to the siblings group
                        mtgSiblings.AddGroup( mtgOffspring );

                        // Hook the offspring group to the previous sibling
                        if( mtgOffspring != null )
                        {
                            mtgOffspring.LeftBox = mtiRightmostSibling;
                        }

                        // If subject is husband then we need to add their wife now.
                        if (fr.IsHusband(irSubject))
                        {
                            ecbCrossbar = CMiniTreeGroup.ECrossbar.DottedRight;

                            // Hook up to previous rightmost sibling and set this as new rightmost sibling.
                            mtiRightmostSibling = AddToGroup(irSpouse, mtgSiblings);

                            // Hook the wife up as box on right of offspring box.
                            if( mtgOffspring != null )
                            {
                                mtgOffspring.RightBox = mtiRightmostSibling;
                            }
                        }

                        nSpouses++;
                    }

                    if (!bAddedSubject)
                    {
                        CBoxText boxtext = new CBoxText(irSubject);
                        CMiniTreeIndividual mtiWife = mtgSiblings.AddIndividual(irSubject, boxtext.m_sFirstName, boxtext.m_sSurname, boxtext.m_sDate, false, frParents != null, true, boxtext.m_bConcealed, false);

                        if (mtgOffspring != null)
                        {
                            mtgOffspring.m_ecCrossbar = CMiniTreeGroup.ECrossbar.Solid;
                            mtgOffspring.RightBox = mtiWife;
                        }
                    }
                } 
                else if( Exists(irSibling) )
                {
                    // A sibling (not the subject).
                    CBoxText boxtext = new CBoxText(irSibling);
                    mtgSiblings.AddIndividual(irSibling, boxtext.m_sFirstName, boxtext.m_sSurname, boxtext.m_sDate, true, frParents != null, true, boxtext.m_bConcealed, false);  
                }

                nSiblings++;
            }
            // End: for each child in frParents.

            // Add siblings group after subject's father
            mtgParents.AddGroup( mtgSiblings );

            // Hook up to subject's father
            mtgSiblings.LeftBox = mtiFather;

            // Add subject's mother
            if( frParents != null )
            {
                CMiniTreeIndividual mtiMother = AddToGroup(frParents.m_xrefWife, mtgParents);
                mtgSiblings.RightBox = mtiMother;
            }

            // Return the parents group (which contains the other family groups).
            return mtgParents;
        }

        // Gets the n'th child in the fr, or returns the default individual if first child requested and no fr.
        private static CIndividualRecord GetChild(CFamilyRecord fr, int nChild, CIndividualRecord irDefault )
        {
            CIndividualRecord irChild = null;
            if (fr != null)
            {
                // The ordering of children in the tree can be selected to be the same as it is in the GEDCOM file. This 
                // is because the file should be ordered as the user chose to order the fr when entering the data in 
                // their fr history app, regardless of actual birth dates. 
                if (MainForm.s_config.m_bKeepSiblingOrder)
                {
                    irChild = fr.GetChildByPositionInFile(nChild);
                }
                else
                {
                    irChild = fr.GetChildByBirthDate(nChild);
                }
            }
            else
            {
                // Return the default individual as first and only child of fr.
                if (nChild == 0)
                {
                    irChild = irDefault;
                }
            }
            return irChild;
        }

        // Add a box for the individual to the specified group.
        private CMiniTreeIndividual AddToGroup(string xref, CMiniTreeGroup mtg)
        {
            return AddToGroup(m_gedcom.GetIndividualRecord(xref), mtg);
        }

        // Add a box for the individual to the specified group.
        private static CMiniTreeIndividual AddToGroup(CIndividualRecord ir, CMiniTreeGroup mtg)
        {
            CMiniTreeIndividual mti = null;
            if (Exists(ir))
            {
                CBoxText boxtext = new CBoxText(ir);
                mti = mtg.AddIndividual(ir, boxtext.m_sFirstName, boxtext.m_sSurname, boxtext.m_sDate, true, false, false, boxtext.m_bConcealed, true);
            }
            else
            {
                mti = mtg.AddIndividual(null, "", MainForm.s_config.m_sUnknownName, " ", false, false, false, false, true);
            }
            return mti;
        }

        // Returns the height of the whole tree diagram.
        public int Height
        {
            get
            {
                return (int)(m_sizeTotal.Height);
            }
        }

        // Returns true if the supplied record is valid for inclusion in the tree
        private static bool Exists( CIndividualRecord ir )
        {
            return (ir != null && ir.Visibility() != CIndividualRecord.EVisibility.Invisible);
        }
    }
}
