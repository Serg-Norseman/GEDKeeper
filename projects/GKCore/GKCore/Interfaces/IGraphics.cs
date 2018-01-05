/*
 *  "GEDKeeper", the personal genealogical database editor.
 *  Copyright (C) 2009-2017 by Sergey V. Zhdanovskih.
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
using BSLib;

namespace GKCore.Interfaces
{
    /// <summary>
    /// Interface for platform-independent class of color representation.
    /// </summary>
    public interface IColor
    {
        IColor Darker(float fraction);
        IColor Lighter(float fraction);
        string GetName();
        int ToArgb();
        string GetCode();
        byte GetR();
        byte GetG();
        byte GetB();
        byte GetA();
        bool IsTransparent();
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IGfxPath : IDisposable
    {
        void AddEllipse(float x, float y, float width, float height);
        void CloseFigure();
        void StartFigure();

        ExtRectF GetBounds();
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IPen : IDisposable
    {
        IColor Color { get; }
        float Width { get; }
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IBrush : IDisposable
    {
        IColor Color { get; }
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IImage : IDisposable
    {
        int Height { get; }
        int Width { get; }
        byte[] GetBytes();
    }


    [Flags]
    public enum ExtFontStyle
    {
        None = 0,
        Bold = 1,
        Italic = 2,
        Underline = 4,
        Strikeout = 8
    }


    /// <summary>
    /// 
    /// </summary>
    public interface IFont : IDisposable
    {
        string FontFamilyName { get; }
        string Name { get; }
        float Size { get; }

        //int Ascent { get; }
        //int Descent { get; }
    }
}
