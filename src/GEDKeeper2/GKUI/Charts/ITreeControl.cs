using System;
using System.Drawing;

namespace GKUI.Charts
{
	/// <summary>
	/// 
	/// </summary>
	public interface ITreeControl : IDisposable
	{
		void Update();
		void Draw(Graphics gfx);
		bool Contains(int x, int y);
		void MouseDown(int x, int y);
		void MouseMove(int x, int y, ThumbMoved thumbMoved);
		void MouseUp(int x, int y);
	}
}
