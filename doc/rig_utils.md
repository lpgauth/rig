

# Module rig_utils #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-decoder">decoder()</a> ###


<pre><code>
decoder() = fun((binary()) -&gt; tuple()) | term
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#change_time-1">change_time/1</a></td><td></td></tr><tr><td valign="top"><a href="#decode_varint-1">decode_varint/1</a></td><td></td></tr><tr><td valign="top"><a href="#encode_varint-1">encode_varint/1</a></td><td></td></tr><tr><td valign="top"><a href="#lookup-3">lookup/3</a></td><td></td></tr><tr><td valign="top"><a href="#match_all-1">match_all/1</a></td><td></td></tr><tr><td valign="top"><a href="#parse_fun-1">parse_fun/1</a></td><td></td></tr><tr><td valign="top"><a href="#read_file-4">read_file/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="change_time-1"></a>

### change_time/1 ###

<pre><code>
change_time(File::<a href="file.md#type-filename">file:filename()</a>) -&gt; pos_integer() | undefined
</code></pre>
<br />

<a name="decode_varint-1"></a>

### decode_varint/1 ###

<pre><code>
decode_varint(Bytes::binary()) -&gt; {integer(), binary()}
</code></pre>
<br />

<a name="encode_varint-1"></a>

### encode_varint/1 ###

<pre><code>
encode_varint(I::integer()) -&gt; binary()
</code></pre>
<br />

<a name="lookup-3"></a>

### lookup/3 ###

<pre><code>
lookup(Key::atom(), List::[{atom(), term()}], Default::term()) -&gt; term()
</code></pre>
<br />

<a name="match_all-1"></a>

### match_all/1 ###

<pre><code>
match_all(X1::'$end_of_table' | {[any()], <a href="ets.md#type-continuation">ets:continuation()</a>}) -&gt; [[any()]]
</code></pre>
<br />

<a name="parse_fun-1"></a>

### parse_fun/1 ###

<pre><code>
parse_fun(Decoder::string()) -&gt; {ok, function()} | {error, invalid_fun}
</code></pre>
<br />

<a name="read_file-4"></a>

### read_file/4 ###

<pre><code>
read_file(File::<a href="file.md#type-io_device">file:io_device()</a>, Decoder::<a href="#type-decoder">decoder()</a>, Tid::<a href="ets.md#type-tid">ets:tid()</a>, KeyElement::pos_integer()) -&gt; ok
</code></pre>
<br />

