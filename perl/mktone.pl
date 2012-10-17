#!/usr/bin/env perl

# usage: mktone.pl | aplay

### GLOBALS ###

$samples_per_sec = 8000;
$pi = 3.14159;

### SUBS ###

sub setup_tables()
{
	my $i;

	$s[0]  = 1.0;
	$s[1]  = 135 / 128;
	$s[2]  = 9 / 8;
	$s[3]  = 6 / 5;
	$s[4]  = 5 / 4;
	$s[5]  = 4 / 3;
	$s[6]  = 45 / 32;
	$s[7]  = 3 / 2;
	$s[8]  = 8 / 5;
	$s[9]  = 27 / 16;
	$s[10] = 9 / 5;
	$s[11] = 15 / 8;

	for ($i = 12; $i <= 96; $i++)
	{
		$s[$i] = $s[$i - 12] * 2;
	}

	$major[0] = 0;
	$major[1] = 2;
	$major[2] = 4;
	$major[3] = 5;
	$major[4] = 7;
	$major[5] = 9;
	$major[6] = 11;

	for ($i = 7; $i <= 64; $i++)
	{
		$major[$i] = $major[$i - 7] + 12;
	}
}

sub play_sound($$$$$$)
{
	my $b_freq = shift;
	my $e_freq = shift;
	my $b_amp = shift;
	my $e_amp = shift;
	my $dur = shift;
	my $func = shift;
	my $sampno, $wavelength;
	my $freq, $amp;
	my $sample;

	$func = sub
	{
		my $sampno = shift;
		my $wavelength = shift;
		return sin($sampno / ($wavelength * $pi)) * 128 + 128;
	} if $func eq 'sine';

	$func = sub
	{
		my $sampno = shift;
		my $wavelength = shift;
		# print STDERR "i $sampno wl $wavelength\n";
		# real modulo.
		my $modulo = $sampno - (int($sampno / $wavelength) * $wavelength);
		return 255 if ($modulo < ($wavelength / 2));
		return 0;
	} if $func eq 'square';

	for ($sampno = 0; $sampno < $dur; $sampno++)
	{
		$freq = ($b_freq + ($e_freq - $b_freq) * ($sampno / $dur));	# XXX logarithms
		$amp = ($b_amp + ($e_amp - $b_amp) * ($sampno / $dur));		# XXX logarithms

		$wavelength = $samples_per_sec / $freq;
		$sample = &$func($sampno, $wavelength) * $amp;
		$sample = 0 if $sample < 0;
		$sample = 255 if $sample > 255;
		print chr(int($sample));
	}
}

sub play_rest($)
{
	my $dur = shift;
	
	for ($i = 0; $i < $dur; $i++)
	{
		print chr(0);
	}
}

sub play_tone($$)
{
	my $note = shift;
	my $dur = shift;

	my $freq1 = $s[$note] * 440;
	# my $freq2 = $s[$note + 1] * 440;
	play_sound($freq1, $freq1, 1.0, 1.0, $dur, 'square');
}

### MAIN ###

setup_tables();
for ($n = 7; $n <= 35; $n++)
{
	play_tone($major[$n], 2000);
	# play_rest(1000);
}
