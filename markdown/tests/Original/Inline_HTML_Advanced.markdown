Simple block on one line:

<div>foo</div>

And nested without indentation:

<div>
<div>
<div>
foo
</div>
<div style=">"/>
</div>
<div>bar</div>
</div>

And with attributes:

<div>
	<div id="foo">
	</div>
</div>

This was broken in 1.0.2b7:

<div class="inlinepage">
<div class="toggleableend">
foo
</div>
</div>
