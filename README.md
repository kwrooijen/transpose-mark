## Transpose Mark

A small libary that lets you transpose data by leaving an Emacs mark on the line you want to transpose.

### Usage

#### Transpose Mark Line

Transpose a marked line with the current line:
`M-x transpose-mark`
or
`M-x transpose-mark-line`

![transpose-mark](https://raw.githubusercontent.com/attichacker/transpose-mark/master/images/transpose-mark.gif)

#### Transpose Mark Region

Select a region to be transposed, run command, afterwards select another region and run command again.
`M-x transpose-mark`
or
`M-x transpose-mark-region`
![transpose-region](https://raw.githubusercontent.com/attichacker/transpose-mark/master/images/transpose-region.gif)

### Functions
* `transpose-mark`
* `transpose-mark-line`
* `transpose-mark-region`

### Faces
* `transpose-mark-region-set-face`
