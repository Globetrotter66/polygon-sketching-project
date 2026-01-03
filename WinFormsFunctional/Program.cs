using System.Collections.Immutable;
using System.Drawing.Drawing2D;

namespace WinFormsFunctional
{
    internal static class Program
    {
        /// <summary>
        ///  The main entry point for the application.
        /// </summary>
        [STAThread]
        static void Main()
        {
            Application.Run(new DrawingForm());
        }
        
        // 1. IMMUTABLE DATA MODEL
         // Using IImmutableList ensures that the data cannot be changed after creation.
        public record CanvasState(
            IImmutableList<IImmutableList<Point>> FinishedPolygons,
            IImmutableList<Point> ActivePolygon
        );

        public class DrawingForm : Form
        {
            // The "Single Source of Truth"
            private CanvasState _currentState;
            private Point _mousePos;

            // History managed as immutable snapshots
            private ImmutableStack<CanvasState> _undoStack = ImmutableStack<CanvasState>.Empty;
            private ImmutableStack<CanvasState> _redoStack = ImmutableStack<CanvasState>.Empty;

            public DrawingForm()
            {
                this.Text = "Pure Functional C# Drawer";
                this.Size = new Size(1000, 750);
                this.DoubleBuffered = true;
                this.KeyPreview = true;

                // Initial State: Nothing on canvas
                _currentState = new CanvasState(
                    ImmutableList<IImmutableList<Point>>.Empty,
                    ImmutableList<Point>.Empty
                );

                SetupToolbar();
            }

            private void SetupToolbar()
            {
                var ts = new ToolStrip { Dock = DockStyle.Top };
                var undoBtn = new ToolStripButton("Undo", null, (s, e) => Undo());
                var redoBtn = new ToolStripButton("Redo", null, (s, e) => Redo());
                ts.Items.AddRange(new ToolStripItem[] { undoBtn, redoBtn });
                this.Controls.Add(ts);
            }

            // 2. FUNCTIONAL STATE TRANSITIONS
            // Instead of modifying lists, we return a NEW state based on the OLD one.

            protected override void OnMouseDown(MouseEventArgs e)
            {
                if (e.Y < 30) return;

                // Push current state to undo before changing
                _undoStack = _undoStack.Push(_currentState);
                _redoStack = _redoStack.Clear();

                if (e.Clicks == 2)
                {
                    // Transition: Move Active to Finished
                    _currentState = _currentState with
                    {
                        FinishedPolygons = _currentState.ActivePolygon.Count > 2
                            ? _currentState.FinishedPolygons.Add(_currentState.ActivePolygon)
                            : _currentState.FinishedPolygons,
                        ActivePolygon = _currentState.ActivePolygon.Count > 2
                            ? ImmutableList<Point>.Empty
                            : _currentState.ActivePolygon
                    };
                }
                else
                {
                    // Transition: Add Point to Active
                    _currentState = _currentState with
                    {
                        ActivePolygon = _currentState.ActivePolygon.Add(e.Location)
                    };
                }

                this.Invalidate();
            }

            private void Undo()
            {
                if (!_undoStack.IsEmpty)
                {
                    _redoStack = _redoStack.Push(_currentState);
                    _undoStack = _undoStack.Pop(out _currentState); // Pops and sets _currentState in one go
                    this.Invalidate();
                }
            }

            private void Redo()
            {
                if (!_redoStack.IsEmpty)
                {
                    _undoStack = _undoStack.Push(_currentState);
                    _redoStack = _redoStack.Pop(out _currentState);
                    this.Invalidate();
                }
            }

            // 3. DECLARATIVE RENDERING
            protected override void OnPaint(PaintEventArgs e)
            {
                var g = e.Graphics;
                g.SmoothingMode = SmoothingMode.AntiAlias;

                // Draw Finished
                foreach (var poly in _currentState.FinishedPolygons)
                    if (poly.Count > 1) g.DrawPolygon(Pens.Black, poly.ToArray());

                // Draw Active
                if (_currentState.ActivePolygon.Count > 0)
                {
                    if (_currentState.ActivePolygon.Count > 1)
                        g.DrawLines(Pens.Blue, _currentState.ActivePolygon.ToArray());

                    g.DrawLine(Pens.Gray, _currentState.ActivePolygon.Last(), _mousePos);
                }
            }

            protected override void OnMouseMove(MouseEventArgs e)
            {
                _mousePos = e.Location;
                if (_currentState.ActivePolygon.Count > 0) this.Invalidate();
            }

            protected override void OnKeyDown(KeyEventArgs e)
            {
                if (e.Control && e.KeyCode == Keys.Z) Undo();
                if (e.Control && e.KeyCode == Keys.Y) Redo();
            }
        }
    }
}