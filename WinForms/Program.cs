using System.Drawing.Drawing2D;

namespace WinForms
{
    internal static class Program
    {
        /// <summary>
        ///  The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            Application.EnableVisualStyles();
            Application.SetCompatibleTextRenderingDefault(false);
            Application.Run(new DrawingCanvas());
        }
        public class CanvasState
        {
            public List<List<Point>> Finished { get; }
            public List<Point> Active { get; }

            public CanvasState(List<List<Point>> finished, List<Point> active)
            {
                // functional feature
                Finished = finished.Select(p => new List<Point>(p)).ToList(); // improve here?
                Active = new List<Point>(active);
            }
        }

        public class DrawingCanvas : Form
        {
            private List<List<Point>> _finishedPolygons = new List<List<Point>>();
            private List<Point> _currentPolygon = new List<Point>();
            private Point _currentMousePos;

            private Stack<CanvasState> _undoStack = new Stack<CanvasState>();
            private Stack<CanvasState> _redoStack = new Stack<CanvasState>();

            public DrawingCanvas()
            {
                this.Text = "2D Drawing App - Point Level Undo/Redo";
                this.Size = new Size(1000, 750);
                this.BackColor = Color.White;
                this.DoubleBuffered = true;
                this.KeyPreview = true;

                InitializeCustomComponents();
            }

            private void InitializeCustomComponents()
            {
                // Create a ToolStrip (Toolbar) for the buttons
                ToolStrip toolStrip = new ToolStrip();
                toolStrip.Dock = DockStyle.Top;

                ToolStripButton undoBtn = new ToolStripButton("Undo (Ctrl+Z)");
                undoBtn.Click += (s, e) => PerformUndo();

                ToolStripButton redoBtn = new ToolStripButton("Redo (Ctrl+Y)");
                redoBtn.Click += (s, e) => PerformRedo();

                ToolStripSeparator separator = new ToolStripSeparator();

                ToolStripButton clearBtn = new ToolStripButton("Clear All");
                clearBtn.Click += (s, e) =>
                {
                    if (_finishedPolygons.Count > 0 || _currentPolygon.Count > 0)
                    {
                        SaveToUndo();
                        _finishedPolygons.Clear();
                        _currentPolygon.Clear();
                        this.Invalidate();
                    }
                };

                toolStrip.Items.Add(undoBtn);
                toolStrip.Items.Add(redoBtn);
                toolStrip.Items.Add(separator);
                toolStrip.Items.Add(clearBtn);

                this.Controls.Add(toolStrip);
            }

            protected override void OnMouseDown(MouseEventArgs e)
            {
                // Ignore clicks if the mouse is over the toolbar
                if (e.Y < 30) return;

                if (e.Button == MouseButtons.Left)
                {
                    SaveToUndo();

                    if (e.Clicks == 2)
                    {
                        if (_currentPolygon.Count > 2)
                        {
                            _finishedPolygons.Add(new List<Point>(_currentPolygon));
                        }
                        _currentPolygon.Clear();
                    }
                    else
                    {
                        _currentPolygon.Add(e.Location);
                    }

                    _redoStack.Clear();
                    this.Invalidate();
                }
            }

            protected override void OnMouseMove(MouseEventArgs e)
            {
                _currentMousePos = e.Location;
                if (_currentPolygon.Count > 0) this.Invalidate();
            }

            protected override void OnPaint(PaintEventArgs e)
            {
                Graphics g = e.Graphics;
                g.SmoothingMode = SmoothingMode.AntiAlias;

                using (Pen polyPen = new Pen(Color.Black, 2))
                using (Pen previewPen = new Pen(Color.CornflowerBlue, 1) { DashStyle = DashStyle.Dash })
                using (Brush fillBrush = new SolidBrush(Color.FromArgb(40, 100, 149, 237)))
                {
                    // Draw finished shapes
                    foreach (var poly in _finishedPolygons)
                    {
                        if (poly.Count > 1)
                        {
                            g.FillPolygon(fillBrush, poly.ToArray());
                            g.DrawPolygon(polyPen, poly.ToArray());
                        }
                    }

                    // Draw active shape
                    if (_currentPolygon.Count > 0)
                    {
                        if (_currentPolygon.Count > 1)
                            g.DrawLines(polyPen, _currentPolygon.ToArray());

                        g.DrawLine(previewPen, _currentPolygon.Last(), _currentMousePos);

                        foreach (var p in _currentPolygon)
                            g.FillEllipse(Brushes.Crimson, p.X - 3, p.Y - 3, 6, 6);
                    }
                }
            }

            private void SaveToUndo()
            {
                // functional feature
                _undoStack.Push(new CanvasState(_finishedPolygons, _currentPolygon));
            }

            private void PerformUndo()
            {
                if (_undoStack.Count > 0)
                {
                    _redoStack.Push(new CanvasState(_finishedPolygons, _currentPolygon));
                    var previous = _undoStack.Pop();
                    _finishedPolygons = previous.Finished;
                    _currentPolygon = previous.Active;
                    this.Invalidate();
                }
            }

            private void PerformRedo()
            {
                if (_redoStack.Count > 0)
                {
                    SaveToUndo();
                    var next = _redoStack.Pop();
                    _finishedPolygons = next.Finished;
                    _currentPolygon = next.Active;
                    this.Invalidate();
                }
            }

            protected override void OnKeyDown(KeyEventArgs e)
            {
                if (e.Control && e.KeyCode == Keys.Z) PerformUndo();
                if (e.Control && e.KeyCode == Keys.Y) PerformRedo();
            }
        }
    }
}