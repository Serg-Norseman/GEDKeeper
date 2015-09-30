using System;
using System.Drawing;
using System.Drawing.Drawing2D;

namespace GKUI.Charts
{
	public interface ITreeControl : IDisposable
	{
		void Update();
		void Draw(Graphics gfx);
		bool Contains(int X, int Y);
		void MouseDown(int X, int Y);
		void MouseMove(int X, int Y, ThumbMoved thumbMoved);
		void MouseUp(int X, int Y);
	}
}
