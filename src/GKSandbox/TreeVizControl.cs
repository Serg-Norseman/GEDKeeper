using System;
using System.Collections.Generic;
using System.Drawing;
using System.Drawing.Imaging;
using System.Windows.Forms;
using System.Timers;

using GedCom551;
using CsGL.OpenGL;
using Ext.ArborEngine;
using Ext.Utils;
using GKCore;
using GKUI;

namespace GKSandbox
{
	public sealed class TreeVizControl : OpenGLControl
	{

		private class TVPerson
		{
			public TGEDCOMIndividualRecord IRec;
			public PointF Pt;
			public int BirthYear, DeathYear;
			public float BaseRadius;
			
			public Node tmpNode;
		}

		private const uint obj_X = 1;
		private const uint obj_Y = 2;
		private const uint obj_Z = 3;
		private const uint obj_Node = 10;
		private const float BaseScale = 50.0f;


		private bool isDisposed = false;
		private int fHeight, fWidth;

		// rendering
		private byte accumDepth = 0;												// OpenGL's Accumulation Buffer Depth, In Bits Per Pixel.
		private byte stencilDepth = 0;											// OpenGL's Stencil Buffer Depth, In Bits Per Pixel.
		private byte zDepth = 16;												// OpenGL's Z-Buffer Depth, In Bits Per Pixel.
		private byte colorDepth = 16;											// The Current Color Depth, In Bits Per Pixel.
		private double nearClippingPlane = 0.1f;									// GLU's Distance From The Viewer To The Near Clipping Plane (Always Positive).
		private double farClippingPlane = 100.0f;								// GLU's Distance From The Viewer To The Far Clipping Plane (Always Positive).
		private double fovY = 45.0f;												// GLU's Field Of View Angle, In Degrees, In The Y Direction.
		private float xrot = 0;
		private float yrot = 0;
		private float zrot = 0;
		private float z = -5.0f;

		// control
		private bool MouseDrag;
		private int LastX, LastY;
		private bool zMode = false;
		public string selObject;

		//private float[] LightAmbient = {0.5f, 0.5f, 0.5f, 1.0f};
		//private float[] LightDiffuse = {1.0f, 1.0f, 1.0f, 1.0f};
		//private float[] LightPosition = {0.0f, 0.0f, 2.0f, 1.0f};
		//private int filter = 0;													// Which Filter To Use
		//private uint[] texture = new uint[3];									// Storage For 3 Textures

		// runtime
		private TfmBase fBase;
		private ArborSystem fSys;
		private List<TVPerson> fPersons;
		private TList fDescList;
		private int fMinYear;
		private float fYearSize;

		private System.Timers.Timer itv = null;
		private bool fBusy;
		private uint fTick;
		private int fCurYear;

		public TreeVizControl()
		{
			GL.glShadeModel(GL.GL_SMOOTH);
			GL.glClearColor(0.0f, 0.0f, 0.0f, 0.5f);
			GL.glClearDepth(1.0f);
			GL.glEnable(GL.GL_DEPTH_TEST);
			GL.glDepthFunc(GL.GL_LEQUAL);
			GL.glHint(GL.GL_PERSPECTIVE_CORRECTION_HINT, GL.GL_NICEST);

			//glEnable(GL_TEXTURE_2D);
			//LoadTextures();
			//GL.glLightfv(GL.GL_LIGHT1, GL.GL_AMBIENT, this.LightAmbient);
			//GL.glLightfv(GL.GL_LIGHT1, GL.GL_DIFFUSE, this.LightDiffuse);
			//GL.glLightfv(GL.GL_LIGHT1, GL.GL_POSITION, this.LightPosition);
			//GL.glEnable(GL.GL_LIGHT1);

			this.Context.Grab();
			OpenGLException.Assert();
		}

		protected override void Dispose(bool disposing)
		{
			if (!isDisposed) {
				if (disposing) {
					GC.SuppressFinalize(this);
				}
				base.Dispose(disposing);
			}
			isDisposed = true;
		}

		~TreeVizControl()
		{
			Dispose(false);
		}

		#region Drawing Routines

		public override void glDraw()
		{
			GL.glClear(GL.GL_COLOR_BUFFER_BIT | GL.GL_DEPTH_BUFFER_BIT);
			GL.glLoadIdentity();

			GL.glInitNames();

			GL.glTranslatef(0.0f, 0.0f, this.z);
			GL.glRotatef(this.xrot, 1.0f, 0.0f, 0.0f);
			GL.glRotatef(this.yrot, 0.0f, 1.0f, 0.0f);
			GL.glRotatef(this.zrot, 0.0f, 0.0f, 1.0f);

			/*glBindTexture(GL_TEXTURE_2D, texture[filter]);
			glBegin(GL_QUADS);
				// Front Face
				glNormal3f(0.0f, 0.0f, 1.0f);
				glTexCoord2f(0.0f, 0.0f); glVertex3f(-1.0f, -1.0f,  1.0f);
				glTexCoord2f(1.0f, 0.0f); glVertex3f( 1.0f, -1.0f,  1.0f);
				glTexCoord2f(1.0f, 1.0f); glVertex3f( 1.0f,  1.0f,  1.0f);
				glTexCoord2f(0.0f, 1.0f); glVertex3f(-1.0f,  1.0f,  1.0f);
				// Back Face
				glNormal3f(0.0f, 0.0f, -1.0f);
				glTexCoord2f(1.0f, 0.0f); glVertex3f(-1.0f, -1.0f, -1.0f);
				glTexCoord2f(1.0f, 1.0f); glVertex3f(-1.0f,  1.0f, -1.0f);
				glTexCoord2f(0.0f, 1.0f); glVertex3f( 1.0f,  1.0f, -1.0f);
				glTexCoord2f(0.0f, 0.0f); glVertex3f( 1.0f, -1.0f, -1.0f);
				// Top Face
				glNormal3f(0.0f, 1.0f, 0.0f);
				glTexCoord2f(0.0f, 1.0f); glVertex3f(-1.0f,  1.0f, -1.0f);
				glTexCoord2f(0.0f, 0.0f); glVertex3f(-1.0f,  1.0f,  1.0f);
				glTexCoord2f(1.0f, 0.0f); glVertex3f( 1.0f,  1.0f,  1.0f);
				glTexCoord2f(1.0f, 1.0f); glVertex3f( 1.0f,  1.0f, -1.0f);
				// Bottom Face
				glNormal3f(0.0f, -1.0f, 0.0f);
				glTexCoord2f(1.0f, 1.0f); glVertex3f(-1.0f, -1.0f, -1.0f);
				glTexCoord2f(0.0f, 1.0f); glVertex3f( 1.0f, -1.0f, -1.0f);
				glTexCoord2f(0.0f, 0.0f); glVertex3f( 1.0f, -1.0f,  1.0f);
				glTexCoord2f(1.0f, 0.0f); glVertex3f(-1.0f, -1.0f,  1.0f);
				// Right face
				glNormal3f(1.0f, 0.0f, 0.0f);
				glTexCoord2f(1.0f, 0.0f); glVertex3f( 1.0f, -1.0f, -1.0f);
				glTexCoord2f(1.0f, 1.0f); glVertex3f( 1.0f,  1.0f, -1.0f);
				glTexCoord2f(0.0f, 1.0f); glVertex3f( 1.0f,  1.0f,  1.0f);
				glTexCoord2f(0.0f, 0.0f); glVertex3f( 1.0f, -1.0f,  1.0f);
				// Left Face
				glNormal3f(-1.0f, 0.0f, 0.0f);
				glTexCoord2f(0.0f, 0.0f); glVertex3f(-1.0f, -1.0f, -1.0f);
				glTexCoord2f(1.0f, 0.0f); glVertex3f(-1.0f, -1.0f,  1.0f);
				glTexCoord2f(1.0f, 1.0f); glVertex3f(-1.0f,  1.0f,  1.0f);
				glTexCoord2f(0.0f, 1.0f); glVertex3f(-1.0f,  1.0f, -1.0f);
			glEnd();*/

			// draw z-axis
			GL.glPushName(obj_Z);
			GL.glColor3f(1.0F, 1.0F, 1.0F);
			GL.glBegin(GL.GL_LINES); // z
				GL.glVertex3f(0, 0, 0);
				GL.glVertex3f(0, 0, 100);
			GL.glEnd();
			GL.glColor3f(0.5F, 0.5F, 0.5F);
			GL.glBegin(GL.GL_LINES); // z
				GL.glVertex3f(0, 0, 0);
				GL.glVertex3f(0, 0, -100);
			GL.glEnd();
			GL.glPopName();
			
			// draw y-axis
			GL.glPushName(obj_Y);
			GL.glColor3f(0.0F, 0.0F, 1.0F);
			GL.glBegin(GL.GL_LINES); // y
				GL.glVertex3f(0, 0, 0);
				GL.glVertex3f(0, 100, 0);
			GL.glEnd();
			GL.glColor3f(0.0F, 0.0F, 0.5F);
			GL.glBegin(GL.GL_LINES); // y
				GL.glVertex3f(0, 0, 0);
				GL.glVertex3f(0, -100, 0);
			GL.glEnd();
			GL.glPopName();

			// draw x-axis
			GL.glPushName(obj_X);
			GL.glColor3f(0.0F, 1.0F, 0.0F);
			GL.glBegin(GL.GL_LINES); // x
				GL.glVertex3f(0, 0, 0);
				GL.glVertex3f(100, 0, 0);
			GL.glEnd();
			GL.glColor3f(0.0F, 0.5F, 0.0F);
			GL.glBegin(GL.GL_LINES); // x
				GL.glVertex3f(0, 0, 0);
				GL.glVertex3f(-100, 0, 0);
			GL.glEnd();
			GL.glPopName();
			
			this.XDrawArbor();
		}

 		private void drawCircle(float radius)
		{
 			const float DEG2RAD = 3.14159F / 180;

			GL.glBegin(GL.GL_LINE_LOOP);
			for (int i = 0; i <= 360; i++) {
				float degInRad = i*DEG2RAD;
				GL.glVertex2f((float)Math.Cos(degInRad) * radius, (float)Math.Sin(degInRad) * radius);
			}
			GL.glEnd();
		}

		private void XDrawArbor()
		{
			if (fSys == null) return;

			try
			{
				// отметки веков
				for (int i = 0; i <= 5; i++) {
					GL.glPushMatrix();
					GL.glTranslatef(0, 0, i * 100 * this.fYearSize);
					GL.glColor3f(0.9F, 0.1F, 0.1F);
					this.drawCircle(0.1F);
					GL.glPopMatrix();
				}

				//RectangleF[] nodeBoxes = new RectangleF[FSys.c_nodes.Count];
				for (int i = 0; i <= fSys.c_nodes.Count - 1; i++) {
					Node node = fSys.c_nodes[i];
					ArbPoint pt = (node.pt);

					GL.glPushName(obj_Node + (uint)i);
					GL.glPushMatrix();
					GL.glTranslatef((float)pt.x, (float)pt.y, 0);
					GL.glColor3f(0.9F, 0.3F, 0.3F);
					this.drawCircle(0.1F);
					GL.glPopMatrix();
					GL.glPopName();

					/*SizeF tsz = bufx.MeasureString(node.sign, FDrawFont);
					float w = tsz.Width + 10;
					ArbPoint pt = FSys.toScreen(node.pt);
					pt.x = Math.Floor(pt.x);
					pt.y = Math.Floor(pt.y);

					bufx.FillRectangle(new SolidBrush(node.color), (float)pt.x - w / 2, (float)pt.y - 10, w, 20);
					nodeBoxes[i] = new RectangleF((float)pt.x - w / 2, (float)pt.y - 11, w, 22);

					bufx.DrawString(node.sign, FDrawFont, new SolidBrush(Color.White), (float)pt.x - tsz.Width / 2, (float)pt.y - tsz.Height / 2);
					 */
				}

				for (int i = 0; i <= fSys.c_edges.Count - 1; i++) {
					Edge edge = fSys.c_edges[i];
					ArbPoint pt1 = (edge.source.pt);
					ArbPoint pt2 = (edge.target.pt);

					GL.glPushMatrix();
					GL.glColor3f(0.9F, 0.3F, 0.3F);
					GL.glBegin(GL.GL_LINES);
					GL.glVertex3f((float)pt1.x, (float)pt1.y, 0);
					GL.glVertex3f((float)pt2.x, (float)pt2.y, 0);
					GL.glEnd();
					GL.glPopMatrix();
				}

				for (int i = 0; i <= fPersons.Count - 1; i++) {
					TVPerson prs = fPersons[i];
					this.DrawPerson(i, prs);

					if (prs.BaseRadius < 100) {
						GL.glPushMatrix();
						GL.glTranslatef((float)prs.Pt.X, (float)prs.Pt.Y, 0);
						GL.glColor3f(0.9F, 0.1F, 0.1F);
						this.drawCircle(prs.BaseRadius);
						GL.glPopMatrix();
					}
				}

				// this is debug, don't delete
				//string energy = "max=" + FSys.energy_max + ", mean=" + FSys.energy_mean + ", thres=" + FSys.energy_threshold;
				//bufx.DrawString(energy, FDrawFont, new SolidBrush(Color.Black), 10, 10);
			} catch (Exception ex) {
				SysUtils.LogWrite("TreeVizControl.XDrawArbor(): " + ex.Message);
			}
		}

		private void DrawPerson(int idx, TVPerson prs)
		{
			if (prs.BirthYear > this.fCurYear) return;

			int endYear;
			if (this.fCurYear < prs.DeathYear) {
				endYear = this.fCurYear;
			} else {
				endYear = prs.DeathYear;
			}

			float zBirth, zDeath;
			zBirth = this.fYearSize * (prs.BirthYear - this.fMinYear);
			zDeath = this.fYearSize * (endYear - this.fMinYear);

			GL.glPushName(obj_Node + (uint)idx);
			GL.glPushMatrix();

			GL.glColor3f(0.1F, 0.7F, 0.7F);

			GL.glBegin(GL.GL_LINES);
			GL.glVertex3f(prs.Pt.X, prs.Pt.Y, zBirth);
			GL.glVertex3f(prs.Pt.X, prs.Pt.Y, zDeath);
			GL.glEnd();

			GL.glPopMatrix();
			GL.glPopName();
		}

		#endregion

		#region Internals

		protected override OpenGLContext CreateContext()
		{
			ControlGLContext context = new ControlGLContext(this);
			DisplayType displayType = new DisplayType(this.colorDepth, this.zDepth, this.stencilDepth, this.accumDepth);
			context.Create(displayType, null);
			return context;
		}

		protected override void OnSizeChanged(EventArgs e)
		{
			Size s = this.Size;
			if (s.Width != 0 && s.Height != 0) {
				this.Reshape((int)s.Width, (int)s.Height);
			}
			base.OnSizeChanged(e);
		}

		private void Reshape(int width, int height)
		{
			fHeight = height;
			fWidth = width;
			
			if (height == 0) {
				height = 1;
			}

			GL.glViewport(0, 0, width, height);
			GL.glMatrixMode(GL.GL_PROJECTION);
			GL.glLoadIdentity();
 
			GL.gluPerspective(this.fovY, (float)width / (float)height, this.nearClippingPlane, this.farClippingPlane);

			GL.glMatrixMode(GL.GL_MODELVIEW);
			GL.glLoadIdentity();
		}

		#endregion

		#region UI Control

		private uint RetrieveObjectID(int x, int y)
		{
			int objectsFound = 0;
			int[] viewportCoords = {0, 0, 0, 0};    // Массив для хранения экранных координат

			// Переменная для хранения ID обьектов, на которые мы кликнули.
			// Мы делаем массив в 32 элемента, т.к. OpenGL также сохраняет другую
			// информацию, которая нам сейчас не нужна. Для каждого обьекта нужно
			// 4 слота.
			uint[] selectBuffer = new uint[32];

			// glSelectBuffer регистрирует массив как буфер выбора обьектов. Первый параметр - размер
			// массива. Второй - сам массив для хранения информации.

			GL.glSelectBuffer(32, selectBuffer);   // Регистрируем буфер для хранения выбранных обьектов

			// Эта функция возвращает информацию о многих вещах в OpenGL. Мы передаём GL_VIEWPOR,
			// чтобы получить координаты экрана. Функция сохранит их в переданном вторым параметром массиве
			// в виде top,left,bottom,right.
			GL.glGetIntegerv(GL.GL_VIEWPORT, viewportCoords); // Получаем текущие координаты экрана

			// Теперь выходим из матрицы GL_MODELVIEW и переходим в матрицу GL_PROJECTION.
			// Это даёт возможность использовать X и Y координаты вместо 3D.

			GL.glMatrixMode(GL.GL_PROJECTION);    // Переходим в матрицу проекции
			GL.glPushMatrix();         // Переходим в новые экранные координаты

			// Эта функция делает так, что фреймбуфер не изменяется при рендере в него, вместо этого
			// происходит запись имён (ID) примитивов, которые были бы отрисованы при режиме
			// GL_RENDER. Информация помещается в selectBuffer.

			GL.glRenderMode(GL.GL_SELECT);    // Позволяет рендерить обьекты без изменения фреймбуфера
			GL.glLoadIdentity();       // Сбросим матрицу проекции

			// gluPickMatrix позволяет создавать матрицу проекции около нашего курсора. Проще говоря,
			// рендерится только область, которую мы укажем (вокруг курсора). Если обьект рендерится
			// в этой области, его ID сохраняется (Вот он, смысл всей функции).
			// Первые 2 параметра - X и Y координаты начала, следующие 2 - ширина и высота области
			// отрисовки. Последний параметр - экранные координаты. Заметьте, мы вычитаем 'y' из
			// НИЖНЕЙ экранной координаты. Мы сделали это, чтобы перевернуть Y координаты.
			// В 3д-пространстве нулевые y-координаты начинаются внизу, а в экранных координатах
			// 0 по y находится вверху. Также передаём регион 2 на 2 пиксела для поиска в нём обьекта.
			// Это может быть изменено как вам удобнее.
			GL.gluPickMatrix(x, viewportCoords[3] - y, 2, 2, viewportCoords);

			// Далее просто вызываем нашу нормальную функцию gluPerspective, точно так же, как
			// делали при инициализации.
			GL.gluPerspective(this.fovY, (float) fWidth / (float) fHeight, this.nearClippingPlane, this.farClippingPlane);

			GL.glMatrixMode(GL.GL_MODELVIEW); // Возвращаемся в матрицу GL_MODELVIEW

			this.glDraw();          // Теперь рендерим выбранную зону для выбора обьекта

			// Если мы вернёмся в нормальный режим рендеринга из режима выбора, glRenderMode
			// возвратит число обьектов, найденных в указанном регионе (в gluPickMatrix()).

			objectsFound = GL.glRenderMode(GL.GL_RENDER); // Вернемся в режим отрисовки и получим число обьектов

			GL.glMatrixMode(GL.GL_PROJECTION);    // Вернемся в привычную матрицу проекции
			GL.glPopMatrix();              // Выходим из матрицы
			GL.glMatrixMode(GL.GL_MODELVIEW);     // Вернемся в матрицу GL_MODELVIEW

			// Теперь нам нужно выяснить ID выбранных обьектов.
			// Если они есть - objectsFound должно быть как минимум 1.

			if (objectsFound > 0)
			{
				// Если мы нашли более 1 обьекта, нужно проверить значения глубины всех
				// выбоанных обьектов. Обьект с МЕНЬШИМ значением глубины - ближайший
				// к нам обьект, значит и щелкнули мы на него. В зависимости от того, что
				// мы программируем, нам могут понадобится и ВСЕ выбранные обьекты (если
				// некоторые были за ближайшим), но в этом уроке мы позаботимся только о
				// переднем обьекте. Итак, как нам получить значение глубины? Оно сохранено
				// в буфере выбора (selectionBuffer). Для каждого обьекта в нем 4 значения.
				// Первое - "число имен в массиве имен на момент события, далее минимум и
				// максимум значений глубины для всех вершин, которые были выбраны при прошлом
				// событии, далее по содержимое массива имен, нижнее имя - первое;
				// ("the number of names in the name stack at the time of the event, followed
				// by the minimum and maximum depth values of all vertices that hit since the
				// previous event, then followed by the name stack contents, bottom name first.") - MSDN.
				// Единстве, что нам нужно - минимальное значение глубины (второе значение) и
				// ID обьекта, переданного в glLoadName() (четвертое значение).
				// Итак, [0-3] - данные первого обьекта, [4-7] - второго, и т.д...
				// Будте осторожны, так как если вы отображаете на экране 2Д текст, он будет
				// всегда находится как ближайший обьект. Так что убедитесь, что отключили вывод
				// текста при рендеринге в режиме GL_SELECT. Я для этого использую флаг, передаваемый
				// в RenderScene(). Итак, получим обьект с минимальной глубиной!

				// При старте установим ближайшую глубину как глубину первого обьекта.
				// 1 - это минимальное Z-значение первого обьекта.
				uint lowestDepth = selectBuffer[1];

				// Установим выбранный обьект как первый при старте.
				// 3 - ID первого обьекта, переданный в glLoadName().
				uint selectedObject = selectBuffer[3];

				// Проходим через все найденные обьекты, начиная со второго (значения первого
				// мы присвоили изначально).
				for (int i = 1; i < objectsFound; i++)
				{
					// Проверяем, не ниже ли значение глубины текущего обьекта, чем предидущего.
					// Заметьте, мы умножаем i на 4 (4 значения на каждый обьект) и прибавляем 1 для глубины.
					if (selectBuffer[(i * 4) + 1] < lowestDepth)
					{
						// Установим новое низшее значение
						lowestDepth = selectBuffer[(i * 4) + 1];

						// Установим текущий ID обьекта
						selectedObject = selectBuffer[(i * 4) + 3];
					}
				}

				return selectedObject;
			}

			return 0;
		}

		private void Screenshot()
		{
			try {
				Image image = Context.ToImage();
				image.Save(@"d:\screenshot.jpg", ImageFormat.Jpeg);
				image.Dispose();
			}
			catch(Exception e) {
				MessageBox.Show(e.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Stop);
			}
		}

		protected override bool IsInputKey(Keys key)
		{
			switch(key) {
				case Keys.Up:
				case Keys.Down:
				case Keys.Right:
				case Keys.Left:
				case Keys.Tab:
					return true;
				default:
					return base.IsInputKey(key);
			}
		}

		protected override void OnKeyDown(KeyEventArgs e)
		{
			switch (e.KeyCode) {
				case Keys.F5: 
					this.Screenshot();
					break;

				case Keys.PageDown:
					z -= 0.5f;
					break;

				case Keys.PageUp:
					z += 0.5f;
					break;
			}
		}

		protected override void OnKeyUp(KeyEventArgs e)
		{
		}

		protected override void OnMouseDown(MouseEventArgs e)
		{
			base.OnMouseDown(e);
			if (!this.Focused) base.Focus();

			if (e.Button == MouseButtons.Left) {
				MouseDrag = true;
				LastX = e.X;
				LastY = e.Y;
			}
		}

		protected override void OnMouseUp(MouseEventArgs e)
		{
			if (e.Button == MouseButtons.Left) {
				MouseDrag = false;
			}
		}

		protected override void OnMouseMove(MouseEventArgs e)
		{
			if (this.MouseDrag) {
				int dx = e.X - this.LastX;
				int dy = e.Y - this.LastY;

				if (!zMode) {
					xrot += 0.001f * dy;
					yrot += 0.001f * dx;
				} else {
					zrot += 0.001f * dx;
				}
			} else {
				uint objectID = this.RetrieveObjectID(e.X, e.Y);

				switch (objectID) {
					case obj_X:
						selObject = "X-axis";
						break;
					case obj_Y:
						selObject = "Y-axis";
						break;
					case obj_Z:
						selObject = "Z-axis";
						break;
					default:
						if (objectID >= obj_Node) {
							int id = (int)(objectID - obj_Node);

							TVPerson prs = fPersons[id];
							selObject = "["+prs.IRec.XRef+"] " + prs.IRec.aux_GetNameStr(true, false)+
								" " + GKUtils.GetLifeStr(prs.IRec) + ", " + 
								prs.BirthYear.ToString() + " - " + prs.DeathYear.ToString();
						} else {
							selObject = "<none>";
						}
						break;
				}
			}
		}

		protected override void OnMouseWheel(MouseEventArgs e)
		{
			if (e.Delta != 0) {
				z += 0.01f * e.Delta;
			}
		}

		#endregion

		public void Redraw()
		{
			OnPaint(null);
		}

		/*private void LoadTextures() {
			string filename = @".\Crate.bmp";
			Bitmap bitmap = null;														// The Bitmap Image For Our Texture
			Rectangle rectangle;														// The Rectangle For Locking The Bitmap In Memory
			BitmapData bitmapData = null;												// The Bitmap's Pixel Data

			// Load The Bitmap
			try {
				bitmap = new Bitmap(filename);											// Load The File As A Bitmap
				bitmap.RotateFlip(RotateFlipType.RotateNoneFlipY);						// Flip The Bitmap Along The Y-Axis
				rectangle = new Rectangle(0, 0, bitmap.Width, bitmap.Height);			// Select The Whole Bitmap
				
				// Get The Pixel Data From The Locked Bitmap
				bitmapData = bitmap.LockBits(rectangle, ImageLockMode.ReadOnly, System.Drawing.Imaging.PixelFormat.Format24bppRgb);

				glGenTextures(3, texture);												// Create 3 Textures

				// Create Nearest Filtered Texture
				glBindTexture(GL_TEXTURE_2D, texture[0]);
				glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
				glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST); 
				glTexImage2D(GL_TEXTURE_2D, 0, (int) GL_RGB8, bitmap.Width, bitmap.Height, 0, GL_BGR_EXT, GL_UNSIGNED_BYTE, bitmapData.Scan0);

				// Create Linear Filtered Texture
				glBindTexture(GL_TEXTURE_2D, texture[1]);
				glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
				glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR); 
				glTexImage2D(GL_TEXTURE_2D, 0, (int) GL_RGB8, bitmap.Width, bitmap.Height, 0, GL_BGR_EXT, GL_UNSIGNED_BYTE, bitmapData.Scan0);

				// Create MipMapped Texture
				glBindTexture(GL_TEXTURE_2D, texture[2]);
				glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);
				glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR); 
				gluBuild2DMipmaps(GL_TEXTURE_2D, (int) GL_RGB8, bitmap.Width, bitmap.Height, GL_BGR_EXT, GL_UNSIGNED_BYTE, bitmapData.Scan0);
			}
			catch(Exception e) {
				// Handle Any Exceptions While Loading Textures, Exit App
				string errorMsg = "An Error Occurred While Loading Texture:\n\t" + filename + "\n" + "\n\nStack Trace:\n\t" + e.StackTrace + "\n";
				MessageBox.Show(errorMsg, "Error", MessageBoxButtons.OK, MessageBoxIcon.Stop);
				App.Terminate();
			}
			finally {
				if(bitmap != null) {
					bitmap.UnlockBits(bitmapData);
					bitmap.Dispose();
				}
			}
		}*/

        public void CreateArborGraph(TfmBase aBase, int minGens, bool loneSuppress)
        {
        	this.fBase = aBase;

			fSys = new ArborSystem(1000, 1000, 0.1, null); //(10000, 1000, 0.1, this);
			fSys.setScreenSize(50, 50);
			fSys.OnStop += Arbor_OnStop;

        	TreeTools.GPLParams gpl_params = new TreeTools.GPLParams();
        	gpl_params.aLinks = true;
        	gpl_params.aDates = false;

        	this.fDescList = new TList(true);

			using (TList patList = new TList(false)) {
				TreeTools.GetPatriarchsList(aBase.Tree, patList, null, minGens, null, gpl_params);

				int num = patList.Count - 1;
				for (int i = 0; i <= num; i++) {
					TPatriarchObj p_obj = patList[i] as TPatriarchObj;

					if ((!loneSuppress) || (loneSuppress && p_obj.HasLinks)) {
						Node node = fSys.addNode(p_obj.IRec.XRef);
						node.data = p_obj;
						p_obj.data = node;
					}
				}

				for (int i = 0; i <= num; i++) {
					TPatriarchObj pat1 = patList[i] as TPatriarchObj;

					for (int k = 0; k < pat1.ILinks.Count; k++) {
						TPatriarchObj pat2 = pat1.ILinks[k];
						Edge edge = fSys.addEdge(pat1.IRec.XRef, pat2.IRec.XRef, 1);
					}
				}
			}

			this.z = -50;

			fSys.start();
        }

		public void Arbor_OnStop(object sender, EventArgs eArgs)
		{
			this.xrot = -75;
			this.yrot = 0.0F;
			this.zMode = true;

			// загрузить из ArborSystem значения точек и сигнатур патриархов
			this.fPersons = new List<TVPerson>();
			this.fMinYear = 0;

			for (int i = 0; i <= fSys.c_nodes.Count - 1; i++) {
				Node node = fSys.c_nodes[i];

				TVPerson patr = new TVPerson();
				patr.Pt = new PointF((float)node.pt.x, (float)node.pt.y);
				patr.IRec = (TGEDCOMIndividualRecord)fBase.Tree.XRefIndex_Find(node.sign);
				patr.BaseRadius = 100;
				patr.tmpNode = node;

				(node.data as TPatriarchObj).data2 = patr;

				this.PreparePerson(patr);

				if (i == 0) {
					this.fMinYear = patr.BirthYear;
				} else {
					if (this.fMinYear > patr.BirthYear) this.fMinYear = patr.BirthYear;
				}

				this.fPersons.Add(patr);
			}

			for (int i = 0; i <= fPersons.Count - 1; i++) {
				TVPerson prs = fPersons[i];
				Node node = prs.tmpNode as Node;

				TPatriarchObj pObj = node.data as TPatriarchObj;
				for (int k = 0; k <= pObj.ILinks.Count - 1; k++) {
					TPatriarchObj destObj = pObj.ILinks[k];
					Node destNode = destObj.data as Node;

					double dx = destNode.pt.x - node.pt.x;
					double dy = destNode.pt.y - node.pt.y;
					double dist = Math.Sqrt(dx * dx + dy * dy);

					float rad = (float)dist * 2/5;
					if (rad < prs.BaseRadius) {
						prs.BaseRadius = rad;
					}

					TVPerson prs2 = (destNode.data as TPatriarchObj).data2 as TVPerson;
					if (rad < prs2.BaseRadius) {
						prs2.BaseRadius = rad;
					}
				}
			}

			int curYear = DateTime.Now.Year;
			int delta = curYear - fMinYear;

			this.fYearSize = /*BaseScale*/ 10.0f / delta;
			this.fTick = 0;
			this.fCurYear = this.fMinYear;

			// fsys.dispose();

			this.startTimer();
		}

		private void PreparePerson(TVPerson prs)
		{
			prs.BirthYear = TreeTools.PL_GetBirthYear(prs.IRec);
			prs.DeathYear = TreeTools.PL_GetDeathYear(prs.IRec);
			if (prs.DeathYear <= 0) prs.DeathYear = prs.BirthYear + 75;
		}

		private void TV_Update(object sender, System.Timers.ElapsedEventArgs e)
		{
			if (this.fBusy) return;
			this.fBusy = true;
			try
			{
				this.zrot -= 0.5f;
				this.fTick += 1;

				if (this.fTick % 2 == 0) {
					this.fCurYear += 1;
				}
			}
			catch (Exception ex)
			{
				SysUtils.LogWrite("TreeVizControl.TV_Update(): " + ex.Message);
			}
			this.fBusy = false;
		}

		public void startTimer()
		{
			if (itv != null) {
				return;
			}

			itv = new System.Timers.Timer();
			itv.AutoReset = true;
			itv.Interval = 50;
			itv.Elapsed += new System.Timers.ElapsedEventHandler(this.TV_Update);
			itv.Start();
		}

		public void stopTimer()
		{
			if (itv != null) {
				itv.Stop();
				itv = null;
			}
		}

	}
}
