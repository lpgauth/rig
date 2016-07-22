

# Module rig #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-key">key()</a> ###


<pre><code>
key() = term()
</code></pre>




### <a name="type-table">table()</a> ###


<pre><code>
table() = atom()
</code></pre>




### <a name="type-value">value()</a> ###


<pre><code>
value() = term()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all-1">all/1</a></td><td></td></tr><tr><td valign="top"><a href="#read-2">read/2</a></td><td></td></tr><tr><td valign="top"><a href="#read-3">read/3</a></td><td></td></tr><tr><td valign="top"><a href="#version-1">version/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all-1"></a>

### all/1 ###

<pre><code>
all(Table::<a href="#type-table">table()</a>) -&gt; {ok, [{<a href="#type-key">key()</a>, <a href="#type-value">value()</a>}]} | {error, unknown_table}
</code></pre>
<br />

<a name="read-2"></a>

### read/2 ###

<pre><code>
read(Table::<a href="#type-table">table()</a>, Key::<a href="#type-key">key()</a>) -&gt; {ok, <a href="#type-value">value()</a>} | {error, unknown_key | unknown_table}
</code></pre>
<br />

<a name="read-3"></a>

### read/3 ###

<pre><code>
read(Table::<a href="#type-table">table()</a>, Key::<a href="#type-key">key()</a>, Default::<a href="#type-value">value()</a>) -&gt; {ok, <a href="#type-value">value()</a>} | {error, unknown_table}
</code></pre>
<br />

<a name="version-1"></a>

### version/1 ###

<pre><code>
version(Table::<a href="#type-table">table()</a>) -&gt; {ok, <a href="ets.md#type-tid">ets:tid()</a>} | {error, unknown_table}
</code></pre>
<br />

